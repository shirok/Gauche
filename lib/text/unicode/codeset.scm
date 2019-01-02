;;;
;;;   text.unicode.codeset - Set of character codes
;;;
;;;    Originally written by Shiro Kawai, 2011
;;;    Public Domain - use as you like.
;;;

;; This module provides char-code-set, a set of character codepoints.
;; Internally it is kept in a bitmap and a collection of character code
;; ranges, just like the built-in <char-set>.
;;
;; We need this instead of built-in <char-set> to generate character
;; category tables.  The internal <char-set> assumes characters
;; are in native encoding, but for the code generation we have to deal
;; with all supported encodings.

(define-module text.unicode.codeset
  (export <char-code-set>
          add-char!
          add-char-range-no-overlap!
          dump-code-set-in-C))
(select-module text.unicode.codeset)

(define-constant SCM_CHAR_SET_SMALL_CHARS 128)

;; <char-code-set> is for an intermediate structure to build <char-set>
;; for each character categories.

(define-class <char-code-set> ()
  ((name :init-keyword :name)
   (small-map :init-value 0)
   (large-map :init-form (make-tree-map))))  ;; integer : integer

(define-method write-object ((cs <char-code-set>) out)
  (print (~ cs'name))
  (do ([k 0 (+ k 32)])
      [(>= k SCM_CHAR_SET_SMALL_CHARS)]
    (let1 word (ash (~ cs'small-map) (- k))
      (format out "~a~8,'0b ~8,'0b ~8,'0b ~8,'0b~a"
              (if (zero? k) "[" " ")
              (logand (ash word -24) #xff)
              (logand (ash word -16) #xff)
              (logand (ash word -8) #xff)
              (logand (ash word 0) #xff)
              (if (>= k (- SCM_CHAR_SET_SMALL_CHARS 32)) "]" "\n"))))
  (dolist [r (tree-map->alist (~ cs'large-map))]
    (format out "\n~5,'0x-~5,'0x" (car r) (cdr r))))

(define-method add-char! ((cs <char-code-set>) code)
  (if (< code SCM_CHAR_SET_SMALL_CHARS)
    (set! (~ cs'small-map) (copy-bit code (~ cs'small-map) #t))
    (receive (l0 h0) (tree-map-floor (~ cs'large-map) code)
      (receive (l1 h1) (tree-map-ceiling (~ cs'large-map) code)
        (cond [(and h0 l1 (= (+ h0 1) code) (= code (- l1 1)))
               ;; merge two
               (tree-map-delete! (~ cs'large-map) l1)
               (tree-map-put! (~ cs'large-map) l0 h1)]
              [(and h0 (= (+ h0 1) code))
               ;; extend right
               (tree-map-put! (~ cs'large-map) l0 code)]
              [(and l1 (= code (- l1 1)))
               ;; extend left
               (tree-map-delete! (~ cs'large-map) l1)
               (tree-map-put! (~ cs'large-map) code h1)]
              [(and l0 (<= code h0))] ;; already a member
              [else                   ;; island
               (tree-map-put! (~ cs'large-map) code code)])))))

;; Add character range that's known not to overlap the existing one.
(define-method add-char-range-no-overlap! ((cs <char-code-set>) start end)
  (when (< start SCM_CHAR_SET_SMALL_CHARS)
    (let1 e (min end SCM_CHAR_SET_SMALL_CHARS)
      (update! (~ cs'small-map) (cut copy-bit-field <> -1 start e))))
  (when (<= SCM_CHAR_SET_SMALL_CHARS end)
    (tree-map-put! (~ cs'large-map) (max SCM_CHAR_SET_SMALL_CHARS start) end)))

;; Dump the set as static C code fragment to construct immutable charset.
(define-method dump-code-set-in-C ((cs <char-code-set>))
  (define name (~ cs'name))
  (define (dump-bitmap bitmap word-size)
    (do ([k 0 (+ k word-size)])
        [(>= k SCM_CHAR_SET_SMALL_CHARS)]
      (format #t " 0x~v,'0xul,\n"
              (/ word-size 4)
              (logand (ash bitmap (- k)) (- (expt 2 word-size) 1)))))
  (print #"static const ScmBits charset_~|name|_small[] = {")
  (print  "#if SIZEOF_LONG == 4")
  (dump-bitmap (~ cs'small-map) 32)
  (print  "#else")
  (dump-bitmap (~ cs'small-map) 64)
  (print  "#endif")
  (print  "};")
  (print #"static const uint32_t charset_~|name|_large[] = {")
  (dolist [p (tree-map->alist (~ cs'large-map))]
    (format #t " 0x~6,'0x,\n" (car p))
    (format #t " 0x~6,'0x,\n" (cdr p)))
  (print  "};")
  (print #"static ScmObj make_charset_~|name|()")
  (print  "{")
  (print #"   return Scm_MakeImmutableCharSet(charset_~|name|_small,")
  (print #"                                   charset_~|name|_large,")
  (print #"                                   ~(* 2 (tree-map-num-entries (~ cs'large-map))));")
  (print  "}"))                                
