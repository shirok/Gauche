;;;
;;; Object code template
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; Object code template is a result of in-process assembler. It contains
;; binary code bytearray, with information to 'patch' parts of the code
;; in the 'link' stage.

(define-module lang.asm.linker
  (use gauche.uvector)
  (use gauche.sequence)
  (use binary.io)
  (use util.match)
  (import gauche.typeutil) ; <c-pointer> etc. We don't want to depend gauche.native-type.
  (export <obj-fragment> make-obj-fragment obj-fragment?
          <obj-template> make-obj-template obj-template?
          <obj-template-labels> make-obj-template-labels obj-template-labels?
          obj-template-labels->alist
          link-templates prelink-template linked-label-offset
          serialize-obj-template dump-obj-template deserialize-obj-template
          ))
(select-module lang.asm.linker)

;; Fragment class.  Holds one contiguous section of assembled machine code.
;;   bytes   - u8vector of assembled bytes (zeros at placeholder holes)
;;   labels  - alist of (symbol . byte-offset), offsets relative to this fragment
;;   patches - list of patch descriptors, offsets relative to this fragment
;;   section - symbol naming the section (e.g. 'text, 'data)
;;   locals  - list of keywords naming local-variable slots referenced
(define-class <obj-fragment> ()
  ((bytes   :init-keyword :bytes   :type <u8vector>)
   (labels  :init-keyword :labels)
   (patches :init-keyword :patches)
   (section :init-keyword :section)
   (locals  :init-keyword :locals  :init-value '())))

(define (make-obj-fragment bytes labels patches section :optional (locals '()))
  (make <obj-fragment>
    :bytes bytes :labels labels :patches patches :section section
    :locals locals))

(define (obj-fragment? x) (is-a? x <obj-fragment>))

;; Template class.  Holds one or more fragments plus shared endianness.
;;   fragments       - list of <obj-fragment>
;;   endian          - one of the endianness symbols; can differ from
;;                     (native-endian) when cross-assembling.
;;   stack-word-size - byte size of one stack slot (e.g. 8 on x86_64); used by
;;                     link-templates to compute SP-relative offsets for locals.
(define-class <obj-template> ()
  ((fragments       :init-keyword :fragments)
   (endian          :init-keyword :endian)
   (stack-word-size :init-keyword :stack-word-size :init-value 0)))

(define (make-obj-template fragments endian :optional (stack-word-size 0))
  (make <obj-template> :fragments fragments :endian endian
        :stack-word-size stack-word-size))

(define (obj-template? x) (is-a? x <obj-template>))

;; Label dictionary.  Abstract label registration and lookup.
(define-class <obj-template-labels> ()
  ((table :init-form (make-hash-table 'eq?))))

(define (make-obj-template-labels)
  (make <obj-template-labels>))

(define (obj-template-labels? x) (is-a? x <obj-template-labels>))

;; add-label! :: <obj-template-labels>, symbol, integer -> <obj-template-labels>
;;   Inserts SYM -> OFFSET.  Signals an error on duplicate.  Returns LABELS.
(define (add-label! labels sym offset)
  (when (hash-table-contains? (~ labels 'table) sym)
    (error "duplicate label:" sym))
  (hash-table-put! (~ labels 'table) sym offset)
  labels)

;; get-label-offset :: <obj-template-labels>, symbol -> integer or #f
(define (get-label-offset labels sym)
  (hash-table-ref/default (~ labels 'table) sym #f))

;; merge-labels! :: <obj-template-labels>, alist -> <obj-template-labels>
;;   Adds every (sym . offset) pair from ALIST into LABELS.  Returns LABELS.
(define (merge-labels! labels alist)
  (dolist [p alist]
    (add-label! labels (car p) (cdr p)))
  labels)

;; obj-template-labels->alist :: <obj-template-labels> -> alist
;;   Returns all entries as ((symbol . offset) ...) sorted by ascending offset.
(define (obj-template-labels->alist labels)
  (sort (hash-table->alist (~ labels 'table)) (^[a b] (< (cdr a) (cdr b)))))

;; TRANSIENT: cond-expand guard allows gen-native.scm to load this module
;; with BUILD_GOSH 0.9.15, which lacks native-ptr-fill!.  Remove after 0.9.16.
(define %native-ptr-fill!
  (cond-expand
    [gauche-0.9.15 (^ _ (error "native-ptr-fill! requires Gauche 0.9.16+"))]
    [else (module-binding-ref 'gauche.typeutil 'native-ptr-fill!)]))

;; fill-native-value! :: u8vector, int, int, <native-type>|<integer>|<top>, val -> ()
;;   Fill BYTES from OFFSET spanning SIZE bytes with the binary representation
;;   of VALUE according to TYPE (always little-endian for numeric types).
;;   TYPE may be the Scheme class <integer> to write VALUE as a signed integer
;;   of exactly SIZE bytes, without requiring an FFI native-type object.
(define (fill-native-value! bytes offset size type value endian)
  (define (bad)
    (error "Unsupported type to use in link-templates:" type))
  (cond
   [(eq? type <integer>)
    (case size
      [(1) (put-s8!  bytes offset value)]
      [(2) (put-s16! bytes offset value endian)]
      [(4) (put-s32! bytes offset value endian)]
      [(8) (put-s64! bytes offset value endian)]
      [else (bad)])]
   [(is-a? type <native-type>)
    (unless (<= (~ type 'size) size)
      (errorf "native type ~s doesn't fit in the patch size ~a" type size))
    (cond
     [(eq? (~ type'super) <integer>)
      (if (~ type'unsigned?)
        (case (~ type'size)
          [(1) (put-u8!  bytes offset value)]
          [(2) (put-u16! bytes offset value endian)]
          [(4) (put-u32! bytes offset value endian)]
          [(8) (put-u64! bytes offset value endian)]
          [else (bad)])
        (case (~ type'size)
          [(1) (put-s8!  bytes offset value)]
          [(2) (put-s16! bytes offset value endian)]
          [(4) (put-s32! bytes offset value endian)]
          [(8) (put-s64! bytes offset value endian)]
          [else (bad)]))]
     [(eq? type <float>)   (put-f32! bytes offset value endian)]
     [(eq? type <double>)  (put-f64! bytes offset value endian)]
     ;; NB: We always fill pointer with native endian, for the
     ;; raw address won't make sense for cross assembling.
     [(or (of-type? type <c-pointer>)
          (of-type? type <c-array>)
          (of-type? type <c-function>)
          (eq? type <c-string>))
      (%native-ptr-fill! bytes offset size type value)]
     [else (bad)])]
   [(eq? type <top>)
    (%native-ptr-fill! bytes offset size type value)]
   [else (bad)]))

;; Patch handler table: maps symbol -> (^ [bytes offset entry])
;;   bytes  - u8vector being patched (mutable copy)
;;   offset - byte offset of the patch location
;;   entry  - result of (assoc-ref params kw), or #f if not supplied
(define *patch-handlers* (make-hash-table 'eq?))

(define (register-patch-handler! key handler)
  (hash-table-put! *patch-handlers* key handler))

;; apply-patches! :: u8vector, symbol, patches, params -> ()
;;   Core patch application loop shared by link-template and link-templates.
;;   Applies patches from PARAMS and returns the finalized byte vector and
;;   label alist.  The template is never mutated; a fresh u8vector is returned.
;;   If POSTAMBLE > 0, the returned vector is extended by that many zero bytes
;;   beyond the template's own bytes (useful for per-call data regions).
;;   Each param entry is either:
;;     (keyword <integer> value)                -- write value as signed integer using patch width
;;     (keyword native-type value)              -- fill at the patch's own offset
;;     (keyword native-type value extra-offset) -- fill at patch-offset + extra-offset
;;     (keyword c-array-type values)            -- fill array starting at patch's offset
;;     (keyword c-array-type values extra-offset) -- fill array starting at patch-offset + extra-offset
;;   The offset form lets callers fill locations derived from a named anchor.
;;   The <integer> type form is used by link-templates for local-variable offsets
;;   and anywhere a plain signed integer suffices without an FFI native-type object.
(define (apply-patches! bytes endian patches params labels)
  (let1 len (uvector-size bytes)
    (define (checked-fill! kw actual ntype val)
      (when (>= actual len)
        (errorf "link-templates: ~s adjusted offset ~a exceeds template size ~a"
                kw actual len))
      (fill-native-value! bytes actual (- len actual) ntype val endian))

    (define (fill-array! kw start atype vals)
      (unless (list? vals)
        (error "c-array parameter value must be a list:" vals))
      (let* ([etype (~ atype 'element-type)]
             [esize (~ etype 'size)])
        (let loop ([vs vals] [off start])
          (unless (null? vs)
            (checked-fill! kw off etype (car vs))
            (loop (cdr vs) (+ off esize))))))

    (dolist [patch patches]
      (match patch
        [(kw base 'label-rel (? integer? end-off))
         ;; cross-section relative jump/reference: resolve target label
         (let1 target-addr (get-label-offset labels kw)
           (unless target-addr
             (errorf "link-templates: label-rel: undefined label ~s" kw))
           (let1 disp (- target-addr end-off)
             ;; TRANSIENT: This can be replaced with ineq macro after
             ;; 0.9.16 release
             (unless (and (<= (- (expt 2 31))  disp)
                          (< disp (expt 2 31)))
               (errorf "link-templates: label-rel: displacement out of range for ~s" kw))
             (put-s32! bytes base disp endian)))]
        [(kw base (? integer? width))
         (dolist [entry (filter (^e (eq? (car e) kw)) params)]
           (match entry
             [(_ (? (^x (of-type? x <c-array>)) atype) vals)
              (fill-array! kw base atype vals)]
             [(_ (? (^x (of-type? x <c-array>)) atype) vals (? integer? xoff))
              (fill-array! kw (+ base xoff) atype vals)]
             [(_ ntype val (? integer? xoff))
              (checked-fill! kw (+ base xoff) ntype val)]
             [(_ ntype val)
              (fill-native-value! bytes base width ntype val endian)]))]
        [(kw offset (? symbol? handler-key))
         (let1 handler (hash-table-get *patch-handlers* handler-key #f)
           (if handler
             (handler bytes offset (assoc-ref params kw))
             (error "unknown patch handler:" handler-key)))]))))

;; uniq-slot-ref : (template ...), slot -> value
;;   Returns (~ template slot).  Additionally, checks if all other templates
;;   have the same value.
(define (uniq-slot-ref tmpls slot)
  (assume-type tmpls (<List> <obj-template> 1))
  (rlet1 v (~ (car tmpls) slot)
    (unless (every (^t (eq? (~ t slot) v)) (cdr tmpls))
      (errorf "templates with mixed value of ~a can't be linked: ~s"
              slot tmpls))))

;; link-templates :: (listof <obj-template>), params :key postamble
;;                   -> u8vector, dict
;;   Merges fragments from all templates, grouped by section in first-seen order,
;;   rebases all label/patch offsets, applies params, and returns
;;   the finalized byte vector and merged label alist.
;;   Local variables declared in fragment 'locals slots are collected, assigned
;;   consecutive SP-relative offsets (slot 0 : -word, slot 1 : -2*word, ...)
;;   using the first template's stack-word-size, and prepended to params as
;;   (keyword <integer> value) entries resolved by apply-patches! normally.
;;   A (:nbytes-local <integer> N) entry is also prepended so prolog/epilog
;;   fragments can patch the stack allocation size.
;;   Sections are reordered: prolog -> text -> epilog -> data -> others
(define (link-templates tmpls params :key (postamble 0))
  (unless (pair? tmpls)
    (error "link-templates: template list must not be empty"))
  (let* ([endian (uniq-slot-ref tmpls 'endian)]
         [word   (uniq-slot-ref tmpls 'stack-word-size)]
         ;; All fragments in input order.
         [frags    (append-map (^t (~ t 'fragments)) tmpls)]
         ;; Collect unique local-variable keywords across all fragments,
         ;; first-seen order.
         [local-vars (delete-duplicates
                      (append-map (^f (~ f 'locals)) frags)
                      eq?)]
         ;; Build (kw <integer> signed-sp-offset) entries and prepend to
         ;; caller params.
         [local-params (map-with-index
                        (^[i kw] (list kw <integer> (* (- (+ i 1)) word)))
                        local-vars)]
         [nlocals    (length local-vars)]
         ;; Prepend :nbytes-local so prolog/epilog fragments can patch
         ;; stack allocation size.
         [all-params `((:nbytes-local ,<integer> ,(* nlocals word))
                       ,@local-params
                       ,@params)]
         ;; Section order: prolog first, epilog after text-like sections,
         ;; then data, then rest.
         [raw-sections (delete-duplicates (map (^f (~ f 'section)) frags) eq?)]
         [sections
          (cond-list [(memq 'prolog raw-sections) => car]
                     [(memq 'text raw-sections) => car]
                     [(memq 'epilog raw-sections) => car]
                     [(memq 'data raw-sections) => car]
                     [#t @ (filter
                            (^s (not (memq s '(prolog text epilog data))))
                            raw-sections)])]
         )
    ;; Walk sections in order.  For each section, walk all fragments (in order)
    ;; and pick those matching the current section, accumulating bytes, labels,
    ;; and patches with rebased offsets.
    (let loop ([secs sections] [offset 0]
               [rev-blists '()] [labels (make-obj-template-labels)] [patches '()])
      (if (null? secs)
        ;; All sections processed: build the final vector and apply params.
        (let* ([byte-list (append-map identity (reverse rev-blists))]
               [total     (+ (length byte-list) postamble)]
               [bytes     (list->u8vector
                           (append byte-list (make-list postamble 0)))])
          (apply-patches! bytes endian patches all-params labels)
          (values bytes labels))
        (let1 sec (car secs)
          (let floop ([fs frags] [off offset]
                      [sec-blist '()] [sec-labels '()] [sec-patches '()])
            (if (null? fs)
              ;; Done with this section's fragments; advance to next section.
              (loop (cdr secs) off
                    (cons (reverse sec-blist) rev-blists)
                    (merge-labels! labels sec-labels)
                    (append patches sec-patches))
              (let1 frag (car fs)
                (if (eq? (~ frag 'section) sec)
                  (let* ([fb   (~ frag 'bytes)]
                         [flen (uvector-size fb)])
                    (floop (cdr fs)
                           (+ off flen)
                           (append (reverse (u8vector->list fb)) sec-blist)
                           (append sec-labels
                                   (map (^p (cons (car p) (+ off (cdr p))))
                                        (~ frag 'labels)))
                           (append sec-patches
                                   (map (^p (match p
                                              [(kw disp 'label-rel end-off)
                                               (list kw (+ off disp) 'label-rel (+ off end-off))]
                                              [(kw disp . rest)
                                               (cons* kw (+ off disp) rest)]))
                                        (~ frag 'patches)))))
                  (floop (cdr fs) off
                         sec-blist sec-labels sec-patches))))))))))

;; prelink-template :: <obj-template>, params -> <obj-template>
;;   Applies patches that can be resolved from PARAMS alone, without
;;   reordering or concatenating fragments.  Label-relative patches and
;;   typed patches with no matching param entry are left in place for a
;;   subsequent link-templates call.  Returns a fresh <obj-template> with
;;   updated fragment bytes and a reduced patch list.
(define (prelink-template tmpl params)
  (assume-type tmpl <obj-template>)
  (let ([endian (~ tmpl 'endian)]
        [word   (~ tmpl 'stack-word-size)])
    (define (applicable? p)
      (match p
        [(_ _ 'label-rel _) #f]
        [(_ _ (? symbol? _)) #t]
        [(kw _ (? integer? _)) (and (assq kw params) #t)]
        [_ #f]))
    (make-obj-template
     (map (^[frag]
            (let-values ([(bytes) (uvector-copy (~ frag 'bytes))]
                         [(frag-labels) (merge-labels! (make-obj-template-labels)
                                                       (~ frag 'labels))]
                         [(apply-now defer) (partition applicable? (~ frag'patches))])
              (apply-patches! bytes endian apply-now params frag-labels)
              (make-obj-fragment bytes
                                 (~ frag 'labels)
                                 defer
                                 (~ frag 'section)
                                 (~ frag 'locals))))
          (~ tmpl 'fragments))
     endian
     word)))

;; linked-label-offset :: label-info, label -> integer
;;   LABEL-INFO is the second value returned from link-templates
;;   (It is currently just an alist, but users should treat it as opaque
;;   structure).
;;   Look up LABEL (symbol) in LABEL-INFO and returns its offset.
;;   Signals an error if the label is not found.
(define (linked-label-offset label-info label)
  (or (get-label-offset label-info label)
      (errorf "linked-label-offset: label not found: ~s" label)))

;; serialize-obj-template :: <obj-template> -> list
;;  Convert an obj-template to a nested keyword-value list for serialization.
(define (serialize-obj-template tmpl)
  (assume-type tmpl <obj-template>)
  `(:fragments
    ,(map (^[frag]
            `(:section ,(~ frag 'section)
              :bytes   ,(~ frag 'bytes)
              :labels  ,(~ frag 'labels)
              :patches ,(~ frag 'patches)
              :locals  ,(~ frag 'locals)))
          (~ tmpl 'fragments))
    :endian          ,(~ tmpl 'endian)
    :stack-word-size ,(~ tmpl 'stack-word-size)))

;; dump-obj-template :: <obj-template>, <symbol>, Port -> ()
(define (dump-obj-template tmpl varname port)
  (pprint `(define ,(unwrap-syntax varname)
             ',(serialize-obj-template tmpl))
            :port port
            ;; TRANSIENT: :radix -> :radix-prefix after the new release
            :controls (make-write-controls :pretty #t :width 75
                                           :base 16 :radix #t)))

;; deserialize-obj-template :: list -> <obj-template>
;;   Deserialize an <obj-template> from a keyword-plist literal of the form
;;   produced by gen-native.scm's emit-tmpl-literal:
;;     (:endian ENDIAN :stack-word-size N :fragments
;;      ((:section SEC :bytes BYTES :labels LABELS :patches PATCHES :locals LOCALS) ...))
;;   All keys are optional; missing keys receive sensible defaults.
(define (deserialize-obj-template spec)
  (make-obj-template
   (map (^[fspec]
          (make-obj-fragment
           (get-keyword :bytes   fspec #u8())
           (get-keyword :labels  fspec '())
           (get-keyword :patches fspec '())
           (get-keyword :section fspec 'text)
           (get-keyword :locals  fspec '())))
        (get-keyword :fragments spec '()))
   (get-keyword :endian          spec 'little-endian)
   (get-keyword :stack-word-size spec 0)))

;;;
;;; Architecture-specific patch handlers
;;;

;; They are placed here instead of architecture-specific assembler
;; module.  This breaks modularity, but allows link-template used
;; without loading the assembler.

;; x86_64
;; Handler translates a symbol (movsd/#f -> #xf2, movss -> #xf3) into a
;; prefix byte at the given offset.
(register-patch-handler!
 'x86_64-movs_
 (^[bytes offset entry]
   (let* ([v   (if entry (cadr entry) 'movsd)]
          [pfx (case v
                 [(movsd) #xf2]
                 [(movss) #xf3]
                 [else (error "movs_ value must be movsd or movss:" v)])])
     (u8vector-set! bytes offset pfx))))
