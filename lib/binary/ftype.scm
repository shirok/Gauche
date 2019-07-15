;;;
;;; binary.ftype - foreign types and foreign objects
;;;
;;;   Copyright (c) 2011-2019  Shiro Kawai  <shiro@acm.org>
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

;; This module provides means to define and manipulate
;; structured binary data.
;;
;; Foreign types and foreign objects
;;
;; A @emph{foreign type}, or @code{ftype}, refers to a way of representing
;; external data used outside the Scheme world.  Ftypes are first-class
;; objects in Gauche.  It can be used to extract or construct structured data
;; from/in a plain bytevector (@code{u8vector}) and read from / write to
;; ports.
;;
;; A @emph{foreign object} , or @code{fobject}, is a Scheme object
;; that packages a chunk of binary data (@emph{storage}) with ftype.
;; It behaves like a Scheme record---it can have slots and you can
;; get and set values.  
;; However, it is possible to share storage among different Scheme objects.
;; For example, you can read binary data (e.g. image file content)
;; into an u8vector, then extract a part of it (e.g. image header)
;; as an fobject.  When you do so, the extracted fobject shares its storage
;; with the original u8vector; you can modify the header of the image
;; content through the extracted fobject.  You may thing fobject as
;; a typed reference to a certain memory.

(define-module binary.ftype
  (use gauche.uvector)
  (use gauche.record)
  (use gauche.sequence)
  (use srfi-1)
  (use srfi-13)
  (use binary.io)
  (use util.match)
  (export
   ;; predefined primitive ftypes
   ftype:schar ftype:uchar  ftype:short ftype:ushort ftype:int ftype:uint
   ftype:long  ftype:ulong  ftype:longlong ftype:ulonglong
   ftype:int8  ftype:uint8  ftype:int16 ftype:uint16 ftype:int32 ftype:uint32
   ftype:int64 ftype:uint64
   ftype:float ftype:double

   bitfield
   
   ;; type metainformation
   ftype ftype-name ftype-size ftype-alignment ftype-endian
   ftype-getter ftype-putter

   ftype:struct ftype:struct-slots
   ftype:slot ftype:slot-name ftype:slot-type ftype:slot-position
   make-fstruct-type
   define-fstruct-type

   fobject fobject? fobject-type fobject-storage fobject-offset
   make-fobject

   fobject-copy fobject-copy!/uv fobject-copy!

   get-fobject put-fobject!
   read-fobject!/uv read-fobject write-fobject/uv write-fobject
   fobject-ref fobject-set! fobject-ref/uv fobject-set!/uv
   ))
(select-module binary.ftype)

;; Metainformation of foreign type.
;;
;;  ftype                base class and predefined primitive types
;;    +-- ftype:struct   user-defined struct
;;    +-- ftype:array    uniform array of ftype instances.
;;    +-- ftype:bitfield integer with limited width
;;   (+-- ftype:pointer  maybe, if we support ffi through this.)
;;   (+-- ftype:function maybe, if we support ffi through this.)
;;

;; The base class.  The constructor is private, since the user code
;; never needs to call it.
(define-record-type ftype %make-ftype #t
  name              ; for diagnostics
  size              ; size in bytes
  alignment         ; natrual alignment of this type.  can be overridden.
  endian            ; #f to obey context.  symbol to enfoce specific endian.
  getter            ; uvector pos endian -> value       ; pos is aligned
  putter            ; uvector pos value endian -> void  ; pos is aligned
  )

;; NB: this may not be needed if we have standard printer for structs.
(define-method write-object ((ftd ftype) port)
  (format port "#<ftype ~a>" (ftype-name ftd)))

(define-record-type (ftype:struct ftype) %make-ftype:struct #t
  slots                      ; an assoc list of name and ftype:slot
  )

(define-record-type (ftype:array ftype) %make-ftype:array #t
  (element-type ftype-element-type)     ; ftype
  (num-elements ftype-num-elements)     ; count
  (element-alignment ftype-element-alignment) ; default to natural alignment
                                              ; of element-type, can be
                                              ; overridden.
  )

;; Auxiliary structure to keep each slot's info of ftype:struct.
;; This isn't a subtype of ftype.
(define-record-type ftype:slot #t #t
  name              ; slot name
  type              ; ftype
  position          ; byte offset
  )

;; Auxiliary structure to keep bitfield slot in ftype:struct.
;; When fstruct is defined, bitfield slot needs to be deginated with
;; bit-width and signed/unsigned.  The initializer of fstruct fills
;; bit-position.
(define-record-type (ftype:bitfield ftype) %make-ftype:bitfield #t
  bit-position                          ; start bit position
  bit-width                             ; width
  )

(define (bitfield width)
  (make-ftype:bitfield #f #f width 0))

(define (make-ftype:bitfield name signed? bitwidth bitpos)
  (let* ([octets (div (+ bitwidth 7) 8)]
         [size (ash 1 (integer-length (- octets 1)))]
         [align size]
         [geti (if signed? get-sint get-uint)]
         [puti (if signed? put-sint! put-uint!)])
    (%make-ftype:bitfield name size align #f
                          (^[uv pos endian]
                            (bit-field (geti uv pos endian)
                                       bitpos
                                       (+ bitpos bitwidth)))
                          (^[uv pos val endian]
                            (puti uv pos 
                                  (copy-bit-field (geti uv pos endian)
                                                  val
                                                  bitpos
                                                  (+ bitpos bitwidth))))
                          bitpos
                          bitwidth)))

;;
;; Foreign object instance.
;;

;; An instance of fobject.
(define-record-type fobject %make-fobject fobject?
  type              ; ftype:struct
  storage           ; u8vector.  NB: The storage may be shared.
  offset            ; byte offset within <storage>
  )

;; NB: this may not be needed if we have standard printer for structs.
(define-method write-object ((fst fobject) port)
  (format port "#<fobject ~a>"
          (ftype-name (fobject-type fst))))

;;========================================================================
;; Primitive ftypes.
;;
(define *%primtype-info* (with-module binary.io (%primitive-type-info)))
(define (%primtype-size  prim-name) (cadr (assq prim-name *%primtype-info*)))
(define (%primtype-align prim-name) (caddr (assq prim-name *%primtype-info*)))

(define (%primtype name get put!)
  (let* ([sname (x->string name)]
         [prim-name (cond [(string-prefix? "u" sname)
                           (string->symbol (string-drop sname 1))]
                          [(eq? 'schar name) 'char] ;special case
                          [else name])])
    (%make-ftype name
                 (%primtype-size prim-name)
                 (%primtype-align prim-name)
                 #f get put!)))

(define (%intproc name n)
  (let1 procs  `((1  ,get-s8  ,get-u8  ,put-s8!  ,put-u8!)
                 (2 ,get-s16 ,get-u16 ,put-s16! ,put-u16!)
                 (4 ,get-s32 ,get-u32 ,put-s32! ,put-u32!)
                 (8 ,get-s64 ,get-u64 ,put-s64! ,put-u64!))
    (list-ref (assq (%primtype-size name) procs) n)))

(define get-schar     get-s8)
(define get-uchar     get-u8)
(define get-short     (%intproc 'short 1))
(define get-ushort    (%intproc 'short 2))
(define get-int       (%intproc 'int 1))
(define get-uint      (%intproc 'int 2))
(define get-long      (%intproc 'long 1))
(define get-ulong     (%intproc 'long 2))
(define get-longlong  (%intproc 'longlong 1))
(define get-ulonglong (%intproc 'longlong 2))
(define get-float     get-f32)
(define get-double    get-f64)

(define put-schar!    put-s8!)
(define put-uchar!    put-u8!)
(define put-short!    (%intproc 'short 3))
(define put-ushort!   (%intproc 'short 4))
(define put-int!      (%intproc 'int 3))
(define put-uint!     (%intproc 'int 4))
(define put-long!     (%intproc 'long 3))
(define put-ulong!    (%intproc 'long 4))
(define put-longlong! (%intproc 'longlong 3))
(define put-ulonglong!(%intproc 'longlong 4))
(define put-float!    put-f32!)
(define put-double!   put-f64!)

(define ftype:schar     (%primtype 'schar     get-schar     put-schar!))
(define ftype:uchar     (%primtype 'uchar     get-uchar     put-uchar!))
(define ftype:short     (%primtype 'short     get-short     put-short!))
(define ftype:ushort    (%primtype 'ushort    get-ushort    put-ushort!))
(define ftype:int       (%primtype 'int       get-int       put-int!))
(define ftype:uint      (%primtype 'uint      get-uint      put-uint!))
(define ftype:long      (%primtype 'long      get-long      put-long!))
(define ftype:ulong     (%primtype 'ulong     get-ulong     put-ulong!))
(define ftype:longlong  (%primtype 'longlong  get-longlong  put-longlong!))
(define ftype:ulonglong (%primtype 'ulonglong get-ulonglong put-ulonglong!))
(define ftype:float     (%primtype 'float     get-float     put-float!))
(define ftype:double    (%primtype 'double    get-double    put-double!))
(define ftype:int8      (%primtype 'int8      get-s8        put-s8!))
(define ftype:uint8     (%primtype 'uint8     get-u8        put-u8!))
(define ftype:int16     (%primtype 'int16     get-s16       put-s16!))
(define ftype:uint16    (%primtype 'uint16    get-u16       put-u16!))
(define ftype:int32     (%primtype 'int32     get-s32       put-s32!))
(define ftype:uint32    (%primtype 'uint32    get-u32       put-u32!))
(define ftype:int64     (%primtype 'int64     get-s64       put-s64!))
(define ftype:uint64    (%primtype 'uint64    get-u64       put-u64!))

;;
;; Utilities
;;

(define-inline (%round val align)       ;assumes align is power of 2.
  (if (or (= val 0) (= align 0))
    val
    (logand (+ val (- align 1)) (lognot (- align 1)))))

(define (%check-size ftype uvector pos which)
  (when (< (- (uvector-size uvector) pos) (ftype-size ftype))
    (case which
      [(src)  (errorf "source uvector too short to extract \
                       fobject of type ~s" ftype)]
      [(dest) (errorf "destination uvector too small to put \
                       fobject of type ~s" ftype)]
      [else   (error "[internal] invalid which argument:" which)])))

(define (%adjust-pos ftype uvector pos alignment)
  (let1 align (or alignment (ftype-alignment ftype))
    (rlet1 pos (%round pos align)
      (%check-size ftype uvector pos 'src))))

(define (%get-slot-desc ftd slot)
  (let1 s (assq slot (ftype:struct-slots ftd))
    (unless s (errorf "ftype ~a does't have such slot: ~s" ftd slot))
    (cdr s)))

;;========================================================================
;; foreign struct building blocks
;;

;; slots :: ((name type) ...)
(define (make-fstruct-type name slots endian alignment)
  (receive [slot-descriptors size] (compute-fstruct-slots slots alignment)
    (rec ftype
      (%make-ftype:struct name size
                          (or alignment (compute-fstruct-alignment slots))
                          endian
                          (rec (fobject-get uv pos endian)
                            (%check-size ftype uv pos 'src)
                            (%make-fobject ftype uv pos))
                          (rec (fobject-put! uv pos val endian)
                            (%check-size ftype uv pos 'dest)
                            (let1 off (fobject-offset val)
                              (uvector-copy! uv pos
                                             (fobject-storage val)
                                             off
                                             (+ off size))))
                          slot-descriptors))))

(define (compute-fstruct-alignment slots)
  (apply max (map (^p (ftype-alignment (cadr p))) slots)))

;; Returns <<[ftype:slot] size>>
(define (compute-fstruct-slots slots alignment)
  (define (do-slot slots ss pos)
    (match slots
      [() (values (reverse ss) pos)]
      [([name type] . slots)
       (let* ([align (if alignment
                       (min alignment (ftype-alignment type))
                       (ftype-alignment type))]
              [pos (%round pos align)])
         (if (ftype:bitfield? type)
           (do-bitfield name type slots ss pos 0)
           (do-slot slots
                    `((,name . ,(make-ftype:slot name type pos)) ,@ss)
                    (+ pos (ftype-size type)))))]))
  (define (do-bitfield name type slots ss pos bitpos)
    (let* ([w (ftype:bitfield-bit-width type)]
           [s `(,name
                . ,(make-ftype:slot name
                                    (make-ftype:bitfield `(bits ,w ,bitpos)
                                                         #f
                                                         bitpos
                                                         w)
                                    pos))])
      (match slots
        [() (do-slot slots (cons s ss) (+ pos (div (+ bitpos 7) 8)))]
        [([name type] . rest)
         (if (ftype:bitfield? type)
           (do-bitfield name type rest (cons s ss) pos (+ bitpos w))
           (do-slot slots (cons s ss) (+ pos (div (+ bitpos 7) 8))))])))
  (do-slot slots '() 0))

;;
;; Generic accessors.
;;

(define (get-fobject ftype uvector pos :optional (endian #f) (alignment #f))
  (let1 pos (%adjust-pos ftype uvector pos alignment)
    ((ftype-getter ftype)
     uvector pos (or endian (ftype-endian ftype)))))

(define (put-fobject! ftype uvector pos val
                      :optional (endian #f) (alignment #f))
  (let1 pos (%adjust-pos ftype uvector pos alignment)
    ((ftype-putter ftype)
     uvector pos val (or endian (ftype-endian ftype)))))

;; NB: If we allow variable fields, we need to interpret the content.
;; It may also be an option to allow specifying endian and/or alignment
;; which is different from the internal representation, and let this
;; procedure to do the conversion.  Still not sure if it is worth.
(define (read-fobject!/uv ftype uvector pos
                          :optional (port (current-input-port)))
  (check-arg u8vector? uvector)
  (%check-size ftype uvector pos 'dest)
  (let* ([size (ftype-size ftype)]
         [end (+ pos size)])
    (let loop ([pos pos]
               [nread 0])
      (let1 nread (+ nread (read-block! uvector port pos end))
        (if (< nread size)
          (loop (+ pos nread) nread)
          nread)))))

(define (read-fobject ftype :optional (port (current-input-port)))
  (let1 v (make-u8vector (ftype-size ftype) 0)
    (read-fobject!/uv ftype v 0 port)
    (get-fobject ftype v 0)))

(define (write-fobject/uv ftype uvector pos
                          :optional (port (current-output-port)))
  (check-arg u8vector? uvector)
  (%check-size ftype uvector pos 'src)
  (write-block uvector port pos (+ pos (ftype-size ftype))))

(define (write-fobject fobject :optional (port (current-output-port)))
  (write-fobject/uv (fobject-type fobject)
                    (fobject-storage fobject)
                    (fobject-offset fobject)
                    port))

;; NB: support endian switch
(define (fobject-ref/uv ftd slot uvector pos)
  (let1 s (%get-slot-desc ftd slot)
    ((ftype-getter (ftype:slot-type s))
     uvector (+ pos (ftype:slot-position s))
     #f)))                              ;endian

(define (fobject-set!/uv ftd slot uvector pos val)
  (let1 s (%get-slot-desc ftd slot)
    ((ftype-putter (ftype:slot-type s))
     uvector (+ pos (ftype:slot-position s)) val
     #f)))                              ;endian

(define (fobject-ref obj slot)
  (fobject-ref/uv (fobject-type obj) slot
                  (fobject-storage obj) (fobject-offset obj)))

(define (fobject-set! obj slot val)
  (fobject-set!/uv (fobject-type obj) slot
                   (fobject-storage obj) (fobject-offset obj) val))

;; Initializes byte vector v, returns fobject of type ftype.
;; TODO: allow constructing fobjects other than fstructs.
(define (init-fobject! ftype v pos . initargs)
  (let loop ([args initargs])
    (cond [(null? args) (%make-fobject ftype v 0)]
          [(null? (cdr args)) (error "odd number of initargs:" initargs)]
          [(not (keyword? (car args)))
           (error "keyword required for initarg, but got:" (car args))]
          [else (let1 slot (string->symbol (keyword->string (car args)))
                  (fobject-set!/uv ftype slot v pos (cadr args)))
                (loop (cddr args))])))

(define (make-fobject ftype . initargs)
  (let1 v (make-u8vector (ftype-size ftype) 0)
    (apply init-fobject! ftype v 0 initargs)))

(define (fobject-copy obj)
  (let ([type (fobject-type obj)]
        [off (fobject-offset obj)])
    (%make-fobject type
                   (u8vector-copy (fobject-storage obj)
                                  off (+ off (ftype-size type)))
                   0)))

;; TODO: Should it be a bytewise copy, or should adjust endian, padding etc?
;; Should it check types, or just blindly copy the vector?

(define (fobject-copy!/uv dest-uvector pos src)
  (%check-size (fobject-type src) dest-uvector pos 'dest)
  (uvector-copy! dest-uvector pos
                 (fobject-storage src)
                 (fobject-offset src)))

(define (fobject-copy! dest src)
  ;; TODO: check type compatibility?
  (let ([dest-vec (fobject-storage dest)]
        [dest-off (fobject-offset dest)])
    (%check-size (fobject-type src) dest-vec dest-off 'dest)
    (uvector-copy! dest-vec dest-off
                   (fobject-storage src)
                   (fobject-offset src))
    dest))

;;========================================================================
;; farray
;;


;;========================================================================
;; High-level macro
;;
;; define-fstruct-type name ctor-name pred-name (slot-spec ...) options ...
;;   Creates ftype:struct, and defines the following procedures.
;;
;;  Constructor & filler
;;
;;   make-NAME . initargs => fstruct
;;   init-NAME! uvector pos . initargs
;;
;;  Accessors and modifiers
;;
;;   NAME-SLOT fstruct  => object
;;   NAME-SLOT-set! fstruct val => void
;;   get-NAME-SLOT  uvector pos :optional endian => object
;;   put-NAME-SLOT! uvector pos val :optional endian
;;
;;  NAME is bound to a newly created fstruct-type
;;
;;  <slot-spec> := (slot-name ftype-expr)    ;FTYPE-EXPR is evaluated.
;;

(define-macro (define-fstruct-type name ctor-name pred-name slots . options)
  (let* ([yname (unwrap-syntax name)]
         [maker (cond
                 [(or (symbol? ctor-name) (identifier? ctor-name)) ctor-name]
                 [(eq? ctor-name #t) (string->symbol #"make-~yname")]
                 [(eq? ctor-name #f) #f]
                 ;; TODO: support inheritance like define-record-type
                 [else (error "invalid define-fstruct-type: ctor-name \
                               must be a symbol or a boolean, but got:"
                              ctor-name)])]
         [pred  (cond
                 [(or (symbol? pred-name) (identifier? pred-name)) pred-name]
                 [(eq? pred-name #t) (string->symbol #"~|yname|?")]
                 [(eq? pred-name #f) #f]
                 [else (error "invalid define-fstruct-type: pred-name \
                               must be a symbol or a boolean, but got:"
                              pred-name)])]
         [initializer (string->symbol #"init-~|yname|!")])
    ;; Kludge to get hygienity
    (define (->id x) ((with-module gauche.internal make-identifier) x
                      (find-module 'binary.ftype) '()))
    (define (make-slot-procs sname type)
      (let ([.ref  (string->symbol #"~|yname|-~|sname|")]
            [.set! (string->symbol #"~|yname|-~|sname|-set!")]
            [.get  (string->symbol #"get-~|yname|-~|sname|")]
            [.put! (string->symbol #"put-~|yname|-~|sname|!")])
        `(begin
           (define (,.ref obj)
             (,(->id 'fobject-ref) obj ',sname))
           (define (,.set! obj val)
             (,(->id 'fobject-set!) obj ',sname val))
           (define (,.get vec pos)
             (,(->id 'fobject-ref/uv) ,name ',sname vec pos))
           (define (,.put! vec pos val)
             (,(->id 'fobject-set!/uv) ,name ',sname vec pos val)))))

    `(begin
       (define ,name (,(->id 'make-fstruct-type) ',yname
                      (list
                       ,@(map (^s `(list ',(unwrap-syntax (car s)) ,(cadr s)))
                              slots))
                      #f #f))
       ,(if maker
          `(define (,maker . initargs)
             (apply ,(->id 'make-fobject) ,name initargs))
          '(begin))
       ,(if pred
          `(define (,pred obj)
             (and (fobject? obj)
                  (eq? (fobject-type obj) ,name)))
          '(begin))
       (define (,initializer v pos . initargs)
         (apply ,(->id 'init-fobject!) ,name v pos initargs))
       ,@(map (^s (make-slot-procs (unwrap-syntax (car s)) (cadr s))) slots)
       )))

;;
;; Debugging aid
;;
(define-method describe-slots ((fd ftype))
  (print "fstruct slots:")
  (format #t " off slot      type\n")
  (dolist [slot (ftype:struct-slots fd)]
    (format #t " ~3d ~10a::~a~%"
            (ftype:slot-position (cdr slot))
            (car slot)
            (ftype-name (ftype:slot-type (cdr slot))))))

