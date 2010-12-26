;;; binary.io -- serializing binary data

;;; Created:    <2003-01-20 23:25:06 foof>
;;; Time-stamp: <2003-01-29 20:53:45 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

;; It is computed that eleven thousand persons have at several times
;; suffered death, rather than submit to break their eggs at the smaller
;; end.  Many hundred large volumes have been published upon this
;; controversy: but the books of the Big-endians have been long
;; forbidden, and the whole party rendered incapable by law of holding
;; employments.
;;          -- from "Gulliver's Travels" by Jonathan Swift

;; This module was originally written by Alex Shinn, in pure Scheme.
;; Shiro Kawai rewrote primitive routines in C for the better performance,
;; renamed them for shorter names, and added uvector access routines.

(define-module binary.io
  (use gauche.uvector)
  (use gauche.record)
  (use srfi-1)
  (use srfi-13)
  (export read-uint read-u8 read-u16 read-u32 read-u64
          read-sint read-s8 read-s16 read-s32 read-s64
          read-ber-integer read-f16 read-f32 read-f64
          write-uint write-u8 write-u16 write-u32 write-u64
          write-sint write-s8 write-s16 write-s32 write-s64
          write-ber-integer write-f16 write-f32 write-f64
          get-u8 get-u16 get-u32 get-u64 get-s8 get-s16 get-s32 get-s64
          get-f16 get-f32 get-f64
          get-u16be get-u16le get-u32be get-u32le get-u64be get-u64le
          get-s16be get-s16le get-s32be get-s32le get-s64be get-s64le
          get-f16be get-f16le get-f32be get-f32le get-f64be get-f64le
          put-u8! put-u16! put-u32! put-u64! put-s8! put-s16! put-s32! put-s64!
          put-f16! put-f32! put-f64!
          put-u16be! put-u16le! put-u32be! put-u32le! put-u64be! put-u64le!
          put-s16be! put-s16le! put-s32be! put-s32le! put-s64be! put-s64le!
          put-f16be! put-f16le! put-f32be! put-f32le! put-f64be! put-f64le!

          ;; ftype stuff
          ftype:schar ftype:uchar ftype:short ftype:ushort ftype:int ftype:uint
          ftype:long ftype:ulong ftype:longlong ftype:ulonglong
          ftype:float ftype:double
          
          make-fstruct-type make-fstruct
          fstruct-copy fstruct-copy!/uv fstruct-copy!

          get-fobject put-fobject!
          read-fobject!/uv read-fobject write-fobject/uv write-fobject
          fstruct-ref fstruct-set! fstruct-ref/uv fstruct-set!/uv

          define-fstruct-type

          describe-fstruct-descriptor
          
          ;; old names
          read-binary-uint
          read-binary-uint8 read-binary-uint16
          read-binary-uint32 read-binary-uint64
          read-binary-sint
          read-binary-sint8 read-binary-sint16
          read-binary-sint32 read-binary-sint64
          read-binary-short  read-binary-ushort
          read-binary-long   read-binary-ulong
          read-binary-float read-binary-double
          write-binary-uint
          write-binary-uint8 write-binary-uint16
          write-binary-uint32 write-binary-uint64
          write-binary-sint
          write-binary-sint8 write-binary-sint16
          write-binary-sint32 write-binary-sint64
          write-binary-short  write-binary-ushort
          write-binary-long   write-binary-ulong
          write-binary-float  write-binary-double
          ))
(select-module binary.io)

(dynamic-load "binary--io")

;;;
;;; config
;;;

(define-constant *bit-size* 2)   ;; hey, you never know :)
(define-constant *byte-size* 8)
(define-constant *byte-magnitude* (expt *bit-size* *byte-size*))
(define-constant *byte-mask* (- *byte-magnitude* 1))
(define-constant *byte-right-shift* (* -1 *byte-size*))

;;;
;;; basic reading
;;;

;; mind-numblingly slow, consider a uvector approach but it doesn't
;; handle endianess
(define (read-uint size :optional (port (current-input-port))
                   (endian (default-endian)))
  (case size
    [(1) (read-u8 port endian)]
    [(2) (read-u16 port endian)]
    [(4) (read-u32 port endian)]
    [(8) (read-u64 port endian)]
    [else
     (let loop ((ls '())
                (cnt 0))
       (if (= cnt size)
         (fold (lambda (a b) (+ a (* b *byte-magnitude*)))
               0
               (if (eq? endian 'big-endian) (reverse ls) ls))
         (let1 byte (read-byte port)
           (if (eof-object? byte)
             byte
             (loop (cons byte ls) (+ cnt 1))))))]
    ))

(define (lognot-small int bytes)
  (logand (lognot int) (- (expt *bit-size* (* *byte-size* bytes)) 1)))

(define (uint->sint int bytes)
  (let ((highbit (- (* *byte-size* bytes) 1)))
    (if (logbit? highbit int)
      (* -1 (+ 1 (lognot-small int bytes)))
      int)))

(define (sint->uint int bytes)
  (if (< int 0)
    (+ 1 (lognot-small (abs int) bytes))
    int))

(define (read-sint size :optional
                   (port (current-input-port))
                   (endian (default-endian)))
  (case size
    [(1) (read-s8 port endian)]
    [(2) (read-s16 port endian)]
    [(4) (read-s32 port endian)]
    [(8) (read-s64 port endian)]
    [else (uint->sint (read-uint size port endian) size)]))

;;;
;;; basic writing
;;;

(define (write-uint size int :optional
                    (port (current-output-port))
                    (endian (default-endian)))
  (case size
    [(1) (write-u8 int port endian)]
    [(2) (write-u16 int port endian)]
    [(4) (write-u32 int port endian)]
    [(8) (write-u64 int port endian)]
    [else
     (let ((ls '()))
       ;; build a list of bytes
       (dotimes (i size)
         (push! ls (logand int *byte-mask*))
         (set! int (ash int *byte-right-shift*)))
       ;; reverse if big-endian
       (unless (eq? endian 'big-endian)
         (set! ls (reverse ls)))
       ;; write the list
       (for-each (cut write-byte <> port) ls))]))

(define (write-sint size int :optional
                    (port (current-output-port))
                    (endian (default-endian)))
  (case size
    [(1) (write-s8 int port endian)]
    [(2) (write-s16 int port endian)]
    [(4) (write-s32 int port endian)]
    [(8) (write-s64 int port endian)]
    [else (write-uint size (sint->uint int size) port endian)]))

;;;
;;; compatibility
;;;

;; These are used in binary.pack for the (unofficial) features to
;; read/write integers in "native" width of C system on the platform.
;; Should be removed soon (after binary.pack rewrite).  Do not use them.
(define read-binary-short  read-s16)
(define read-binary-ushort read-u16)
(define read-binary-long   read-s32)
(define read-binary-ulong  read-u32)

(define write-binary-short  write-s16)
(define write-binary-ushort write-u16)
(define write-binary-long   write-s32)
(define write-binary-ulong  write-u32)

;; Other compatibility names.  They have been official befor 0.8.10,
;; and used widely.  So we keep them for a while.
(define read-binary-uint  read-uint)
(define read-binary-sint  read-sint)
(define read-binary-uint8 read-u8)
(define read-binary-sint8 read-s8)
(define read-binary-uint16 read-u16)
(define read-binary-sint16 read-s16)
(define read-binary-uint32 read-u32)
(define read-binary-sint32 read-s32)
(define read-binary-uint64 read-u64)
(define read-binary-sint64 read-s64)
(define read-binary-float  read-f32)
(define read-binary-double read-f64)

(define write-binary-uint  write-uint)
(define write-binary-sint  write-sint)
(define write-binary-uint8 write-u8)
(define write-binary-sint8 write-s8)
(define write-binary-uint16 write-u16)
(define write-binary-sint16 write-s16)
(define write-binary-uint32 write-u32)
(define write-binary-sint32 write-s32)
(define write-binary-uint64 write-u64)
(define write-binary-sint64 write-s64)
(define write-binary-float  write-f32)
(define write-binary-double write-f64)

;;;
;;; bignum encodings -- Basic Encoding Rules (BER) from X.209
;;;

;; A BER compressed integer is an unsigned integer in base 128, most
;; significant digit first, where the high bit is set on all but the
;; final (least significant) byte.  Thus any size integer can be
;; encoded, but the encoding is efficient and small integers don't take
;; up any more space than they would in normal char/short/int encodings.

(define (read-ber-integer :optional (port (current-input-port)))
  (let1 first (read-byte port)
    (if (eof-object? first)
      first ;; stop on eof
      (if (< first 128)
        first
        (let loop ((res (ash (logand first #b01111111) 7))
                   (byte (read-u8 port)))
          (if (< byte 128)
            (+ res byte) ;; final byte
            (loop (ash (+ res (logand byte #b01111111)) 7)
                  (read-u8 port))))))))

(define (write-ber-integer number :optional (port (current-output-port)))
  (let ((final (logand number #b01111111))
        (start (ash number -7)))
    (unless (zero? start)
      (let loop ((n start))
        (cond [(< n 128) (write-u8 (logior n #b10000000))]
              [else
               (loop (ash n -7)) ;; write high bytes first
               (write-u8 (logior (logand n #b01111111) #b10000000))])))
    (write-u8 final)))

;;;
;;;  Foreign types (EXPERIMENTAL)
;;;

;; Metainformation of foreign type.
(define-record-type ftype-descriptor #t #t
  name              ; for diagnostics
  size              ; size in bytes
  alignment         ; natrual alignment of this type.  can be overridden.
  endian            ; #f to obey context.  symbol to enfoce specific endian.
  get               ; uvector pos endian -> value       ; pos is aligned
  put!              ; uvector pos value endian -> void  ; pos is aligned
  )

(define-record-type (fstruct-descriptor ftype-descriptor)
  %make-fstruct-descriptor #t
  slots             ; a list of assoc list of name and fstruct-slot.
  )

;; NB: this may not be needed if we have standard printer for structs.
(define-method write-object ((ftd ftype-descriptor) port)
  (format port "#<ftype-descriptor ~a>" (ftype-descriptor-name ftd)))

(define-record-type fstruct-slot #t #t
  name              ; slot name
  type              ; ftd, fstruct-bitfields or fstruct-array
  position          ; byte offset
  )

;; Bitfields are aggregated to this type.
(define-record-type fstruct-bitfields #t #t
  width             ; total # of octets (including padding) used
  fields            ; list of (name ftd bit-width)
  )

;; Array
(define-record-type fstruct-array #t #t
  length            ; # of entries.
  element-type      ; ftd, fstruct-bitfields or fstruct-array
  )

;; An instance of fstruct.  
(define-record-type fstruct %make-fstruct fstruct?
  type              ; fstruct-type-descrptor
  storage           ; u8vector.  NB: The storage may be shared.
  offset            ; byte offset within <storage>
  endian            ; overridden endian, or #f.
  )

;; NB: this may not be needed if we have standard printer for structs.
(define-method write-object ((fst fstruct) port)
  (format port "#<fstruct ~a>"
          (ftype-descriptor-name (fstruct-type fst))))

;; Primitive ftypes.
(define *%primtype-info* (%primitive-type-info))
(define (%primtype-size  prim-name) (cadr (assq prim-name *%primtype-info*)))
(define (%primtype-align prim-name) (caddr (assq prim-name *%primtype-info*)))

(define (%primtype name get put!)
  (let* ([sname (x->string name)]
         [prim-name (cond [(string-prefix? "u" sname)
                           (string->symbol (string-drop sname 1))]
                          [(eq? 'schar name) 'char] ;special case
                          [else name])])
    (make-ftype-descriptor name
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
  (when (< (- (uvector-size uvector) pos) (ftype-descriptor-size ftype))
    (case which
      [(src)  (errorf "source uvector too short to extract \
                       fstruct of type ~s" ftype)]
      [(dest) (errorf "destination uvector too small to put \
                       fstruct of type ~s" ftype)]
      [else   (error "[internal] invalid which argument:" which)])))

(define (%adjust-pos ftype uvector pos alignment)
  (let1 align (or alignment (ftype-descriptor-alignment ftype))
    (rlet1 pos (%round pos align)
      (%check-size ftype uvector pos 'src))))

(define (%get-slot-desc ftd slot)
  (let1 s (assq slot (fstruct-descriptor-slots ftd))
    (unless s (errorf "fstruct type ~a does't have such slot: ~s" ftd slot))
    (cdr s)))

;;
;; fstruct building blocks
;;

;; slots :: ((name type) ...)
(define (make-fstruct-type name slots endian alignment)
  (receive (size slot-descriptors) (compute-fstruct-slots slots alignment)
    (rec ftype
      (%make-fstruct-descriptor name size
                                (or alignment (compute-fstruct-alignment slots))
                                endian
                                (rec (fstruct-get uv pos endian)
                                  (%check-size ftype uv pos 'src)
                                  (%make-fstruct ftype uv pos endian))
                                (rec (fstruct-put! uv pos val endian)
                                  (%check-size ftype uv pos 'dest)
                                  (let1 off (fstruct-offset val)
                                    (uvector-copy! uv pos
                                                   (fstruct-storage val)
                                                   off
                                                   (+ off size))))
                                slot-descriptors))))

(define (compute-fstruct-alignment slots)
  (apply max (map (^p (ftype-descriptor-alignment (cadr p))) slots)))

(define (compute-fstruct-slots slots alignment)
  (let loop ([slots slots]
             [pos 0]
             [descs '()])
    (if (null? slots)
      (values pos (reverse descs))
      (let* ([name (caar slots)]
             [type (cadar slots)]
             [align (if alignment
                      (min alignment (ftype-descriptor-alignment type))
                      (ftype-descriptor-alignment type))]
             [pos (%round pos align)])
        (loop (cdr slots)
              (+ pos (ftype-descriptor-size type))
              (acons name (make-fstruct-slot name type pos) descs))))))

;;
;; Generic accessors.
;;

(define (get-fobject ftype uvector pos :optional (endian #f) (alignment #f))
  (let1 pos (%adjust-pos ftype uvector pos alignment)
    ((ftype-descriptor-get ftype)
     uvector pos (or endian (ftype-descriptor-endian ftype)))))

(define (put-fobject! ftype uvector pos val
                      :optional (endian #f) (alignment #f))
  (let1 pos (%adjust-pos ftype uvector pos alignment)
    ((ftype-descriptor-put! ftype)
     uvector pos val (or endian (ftype-descriptor-endian ftype)))))

;; NB: If we allow variable fields, we need to interpret the content.
;; It may also be an option to allow specifying endian and/or alignment
;; which is different from the internal representation, and let this
;; procedure to do the conversion.  Still not sure if it is worth.
(define (read-fobject!/uv ftype uvector pos
                          :optional (port (current-input-port)))
  (check-arg u8vector? uvector)
  (%check-size ftype uvector pos 'dest)
  (let* ([size (ftype-descriptor-size ftype)]
         [end (+ pos size)])
    (let loop ([pos pos]
               [nread 0])
      (let1 nread (+ nread (read-block! uvector port pos end))
        (if (< nread size)
          (loop (+ pos nread) nread)
          nread)))))

(define (read-fobject ftype :optional (port (current-input-port)))
  (let1 v (make-u8vector (ftype-descriptor-size ftype) 0)
    (read-fobject!/uv ftype v 0 port)
    (get-fobject ftype v 0)))

(define (write-fobject/uv ftype uvector pos
                          :optional (port (current-output-port)))
  (check-arg u8vector? uvector)
  (%check-size ftype uvector pos 'src)
  (write-block uvector port pos (+ pos (ftype-descriptor-size ftype))))

(define (write-fobject fobject :optional (port (current-output-port)))
  (write-fobject/uv (fstruct-type fobject)
                    (fstruct-storage fobject)
                    (fstruct-offset fobject)
                    port))

;; NB: support endian switch
(define (fstruct-ref/uv ftd slot uvector pos)
  (let1 s (%get-slot-desc ftd slot)
    ((ftype-descriptor-get (fstruct-slot-type s))
     uvector (+ pos (fstruct-slot-position s)))))

(define (fstruct-set!/uv ftd slot uvector pos val)
  (let1 s (%get-slot-desc ftd slot)
    ((ftype-descriptor-put! (fstruct-slot-type s))
     uvector (+ pos (fstruct-slot-position s)) val)))

(define (fstruct-ref fstruct slot)
  (fstruct-ref/uv (fstruct-type fstruct) slot
                  (fstruct-storage fstruct) (fstruct-offset fstruct)))

(define (fstruct-set! fstruct slot val)
  (fstruct-ref/uv (fstruct-type fstruct) slot
                  (fstruct-storage fstruct) val (fstruct-offset fstruct)))

(define (init-fstruct! ftype v pos . initargs)
  (let loop ([args initargs])
    (cond [(null? args) (%make-fstruct ftype v 0 #f)] ;TODO: endian?
          [(null? (cdr args)) (error "odd number of initargs:" initargs)]
          [(not (keyword? (car args)))
           (error "keyword required for initarg, but got:" (car args))]
          [else (let1 slot (string->symbol (keyword->string (car args)))
                  (fstruct-set!/uv ftype slot v pos (cadr args)))
                (loop (cddr args))])))

(define (make-fstruct ftype . initargs)
  (let1 v (make-u8vector (ftype-descriptor-size ftype) 0)
    (apply init-fstruct! ftype v 0 initargs)))

(define (fstruct-copy fstruct)
  (let ([type (fstruct-type fstruct)]
        [off (fstruct-offset fstruct)])
    (%make-fstruct type
                   (u8vector-copy (fstruct-storage fstruct)
                                  off (+ off (ftype-descriptor-size type)))
                   off
                   (fstruct-endian fstruct))))

;; TODO: Should it be a bytewise copy, or should adjust endian, padding etc?
;; Should it check types, or just blindly copy the vector?

(define (fstruct-copy!/uv dest-uvector pos src-fstruct)
  (%check-size (fstruct-type src-fstruct) dest-uvector pos 'dest)
  (uvector-copy! dest-uvector pos
                 (fstruct-storage src-fstruct)
                 (fstruct-offset src-fstruct)))

(define (fstruct-copy! dest-fstruct src-fstruct)
  ;; TODO: check type compatibility?
  (let ([dest (fstruct-storage dest-fstruct)]
        [dest-off (fstruct-offset dest-fstruct)])
    (%check-size (fstruct-type src-fstruct) dest dest-off 'dest)
    (uvector-copy! dest dest-off
                   (fstruct-storage src-fstruct)
                   (fstruct-offset src-fstruct))))

;;
;; High-level macro
;;
;; define-fstruct-type name (slot-spec ...) options ...
;;   Creates fstruct-descriptor, and defines the following procedures.
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
;;  NAME is bound to a newly created fstruct-descriptor.
;;
;;  <slot-spec> := (slot-name ftype-expr)    ;FTYPE-EXPR is evaluated.
;;

(define-macro (define-fstruct-type name slots . options)
  (let* ([yname (unwrap-syntax name)]
         [maker (string->symbol #`"make-,yname")]
         [initializer (string->symbol #`"init-,|yname|!")])
    ;; Kludge to get hygienity
    (define (->id x) ((with-module gauche.internal make-identifier) x
                      (find-module 'binary.io) '()))
    (define (make-slot-procs sname type)
      (let ([.ref  (string->symbol #`",|yname|-,|sname|")]
            [.set! (string->symbol #`",|yname|-,|sname|-set!")]
            [.get  (string->symbol #`"get-,|yname|-,|sname|")]
            [.put! (string->symbol #`"put-,|yname|-,|sname|!")])
        `(begin
           (define (,.ref obj)
             (,(->id 'fstruct-ref) obj ',sname))
           (define (,.set! obj val)
             (,(->id 'fstruct-set!) obj ',sname val))
           (define (,.get vec pos)
             (,(->id 'fstruct-ref/uv) ,name ',sname vec pos))
           (define (,.put! vec pos val)
             (,(->id 'fstruct-set!/uv) ,name ',sname vec pos val)))))

    `(begin
       (define ,name (,(->id 'make-fstruct-type) ',yname
                      (list
                       ,@(map (^s `(list ',(unwrap-syntax (car s)) ,(cadr s)))
                              slots))
                      #f #f))
       (define (,maker . initargs)
         (apply ,(->id 'make-fstruct) ,name initargs))
       (define (,initializer v pos . initargs)
         (apply ,(->id 'init-fstruct!) ,name v pos initargs))
       ,@(map (^s (make-slot-procs (unwrap-syntax (car s)) (cadr s))) slots)
       )))

;;
;; Debugging aid
;;
(define (describe-fstruct-descriptor fd)
  (print "fstruct slots:")
  (dolist [slot (fstruct-descriptor-slots fd)]
    (format #t " ~3d ~10a::~a~%"
            (fstruct-slot-position (cdr slot))
            (car slot)
            (ftype-descriptor-name (fstruct-slot-type (cdr slot))))))

