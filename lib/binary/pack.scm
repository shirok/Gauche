;;;; binary.pack -- packing and unpacking binary data

;;; Author:     Alex Shinn <foof@synthcode.com>

;; It is really insupportable that every hen lays an egg of a different
;; size!  What symmetry can there be on the breakfast table?  At least
;; they should sort them in dozens at the shop!
;;          -- Hercule Poirot, "The Disappearance of Mr. Davenheim"

;;; Disclaimer:
;;   The following code is horribly complicated.  The primary reason is
;;   the fact that I wanted to support the @ pack code while only
;;   performing the required byte position counting when actually using
;;   an @ following a variable length template, without which all the
;;   "vlp" references would be gone.

(define-module binary.pack
  (use srfi-11)   ;; let-values
  (use srfi-13)   ;; string library
  (use srfi-14)   ;; char-set library
  (use text.parse)
  (use gauche.uvector)
  (use gauche.parameter)
  (use binary.io)
  (export pack unpack unpack-skip make-packer))
(select-module binary.pack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general io utilities

(define (read-number)
  (let loop ((c (peek-char))
             (ls '()))
    (if (and (char? c) (char-numeric? c))
      (begin
        (read-char)
        (loop (peek-char) (cons c ls)))
      (string->number (reverse-list->string ls)))))

(define (string-byte-for-each proc str)
  (with-input-from-string str
    (lambda ()
      (let loop ((i (read-byte)))
        (unless (eof-object? i)
          (proc i)
          (loop (read-byte)))))))

(define (while-input-from-string str proc)
  (with-input-from-string str
    (lambda ()
      (until (eof-object? (peek-char))
        (proc)))))

(define (fold-char-list char-list)
  (cond
   ((char-set? char-list) (values char-list #f))
   ((list? char-list)
    (let ((cs (char-set-copy char-set:empty))
          (eof-allowed? #f))
      (for-each (lambda (item)
                  (cond ((char? item) (char-set-adjoin! cs item))
                        ((char-set? item) (char-set-union! cs item))
                        ((eq? item '*eof*) (set! eof-allowed? #t))
                        (else (error "CHAR-LIST must be a list of characters, character sets and/or symbol '*eof*, but found" item))))
                char-list)
      (values cs eof-allowed?)))
   (else (error "CHAR-LIST must be a char-set or a list of characters, char-sets and/or symbol '*eof*" char-list))))

;; text.parse's next-token with a limit
(define (next-token-n prefix-char-list/pred break-char-list/pred limit
                      :optional (comment "unexpected EOF") (port (current-input-port)))
  (define (bad) (errorf "~a~a" (port-position-prefix port) comment))
  (let ((c (skip-while prefix-char-list/pred port)))
    (cond
     ((procedure? break-char-list/pred)
      (with-output-to-string
        (lambda ()
          (let loop ((c c)
                     (i 0))
            (cond ((break-char-list/pred c))
                  ((eof-object? c) (bad))
                  ((= i limit))
                  (else
                   (display (read-char port))
                   (loop (peek-char port) (+ i 1)))))))
      )
     (else
      (receive (cs eof-ok?) (fold-char-list break-char-list/pred)
        (with-output-to-string
          (lambda ()
            (let loop ((c c)
                       (i 0))
              (cond ((eof-object? c) (unless eof-ok? (bad)))
                    ((char-set-contains? cs c))
                    ((= i limit))
                    (else (display (read-char port))
                          (loop (peek-char port) (+ i 1))))))))
      ))))

;; same as above but consumes the break-char
(define (next-token-n* prefix-char-list/pred break-char-list/pred limit
                       :optional (comment "unexpected EOF") (port (current-input-port)))
  (let ((res (next-token-n prefix-char-list/pred
                           break-char-list/pred limit comment port)))
    (if (< (string-size res) limit)
      (read-char port))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numeric utilities

;; these two only operate on the low-byte of a number

(define (bit-for-each proc int)
  (do ((i 0 (+ i 1)))
      ((= i 8))
    (if (logbit? i int) (proc 1) (proc 0))))

(define (bit-reverse-for-each proc int)
  (do ((i 7 (- i 1)))
      ((= i -1))
    (if (logbit? i int) (proc 1) (proc 0))))

(define (hex-char->number c)
  (if (< c 16)
    c
    (or (digit->integer (integer->char c) 16) 0)))

(define (number-writer writer . opt-endian)
  (if (pair? opt-endian)
    (cute writer <> #f (car opt-endian))
    writer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser utilities

(define (get-splitter count . opt-pad)
  (if (not (number? count))
    (cut values <> '())
    (if (pair? opt-pad)
      (cute split-at* <> count #t (car opt-pad))
      (cut split-at* <> count #t))))

(define (read-bang)
  (and (equal? (char->integer #\!) (peek-byte)) (read-byte) #t))

(define (read-count)
  (skip-pack-comments)
  (let ((c (peek-char)))
    (if (char? c)
      (cond ((char-numeric? c)
             (read-number))
            ((eq? c #\*)
             (begin (read-char) c))
            ((eq? c #\[)
             (read-char)
             (let* ((packers (read-until-token (make-pack-token
                                                (char-complement c))
                                               0 #f #f))
                    (p (fold pack-merge-folder #f packers)))
               (if (p 'variable-length?)
                 (error "can't use variable length pack format in []")
                 (p 'length))))
            (else
             1))
      1)))

(define (get-count unpacked-val)
  (x->number (car unpacked-val)))

(define (get-string-padder count . opt-pad)
  (let ((pad #f))
    (if (eq? count #\*)
      (set! count #f)
      (if (pair? opt-pad) (set! pad (car opt-pad))))
    (if pad
      (if (procedure? count)
        (let ((packer (count 'packer)))
          ;; size is dynamic, so we leave it as is and pack the size
          ;; prior to returning the value
          (lambda (str) (packer (list (string-size str))) str))
        (cut string-pad-right <> count pad))
      (cond ((procedure? count)
             (let ((packer (count 'packer)))
               (lambda (str) (packer (list (string-size str))) str)))
            (count
             (cut string-pad-right <> count))
            (else
             identity)))))

(define (make-port-byte-iterator proc count)
  (cond
    ((eq? count #\*)
     (lambda ()
       (list
        (with-output-to-string
          (cut generator-for-each proc read-byte)))))
    ((procedure? count)
     (let ((unpacker (count 'unpacker)))
       (lambda ()
         (let ((actual-count (get-count (unpacker))))
           (list
            (with-output-to-string
              (lambda ()
                (let loop ((i 1))
                  (let ((c (read-byte)))
                    (unless (eof-object? c)
                      (proc c)
                      (unless (= i actual-count)
                        (loop (+ i 1)))))))))))))
    (else
     (lambda ()
       (list
        (with-output-to-string
          (lambda ()
            (let loop ((i 1))
              (let ((c (read-byte)))
                (unless (eof-object? c)
                  (proc c)
                  (unless (= i count)
                    (loop (+ i 1)))))))))))))

;; Skip a value of a given length n, where n could be an integer,
(define (make-skipper n)
  (cond
    ((eq? n #\*)
     (lambda ()
       (while (not (eof-object? (read-block 1024))))))
    ((procedure? n)
     (let ((unpacker (n 'unpacker)))
       (lambda ()
         (let ((count (unpacker)))
           (or (port-seek (current-input-port) count SEEK_CUR)
               (read-block count))))))
    (else
     (lambda ()
       (or (port-seek (current-input-port) n SEEK_CUR)
           (read-block n))))))

(define (char-complement ch)
  (case ch
    ((#\() #\))   ((#\)) #\()
    ((#\[) #\])   ((#\]) #\[)
    ((#\{) #\})   ((#\}) #\{)
    ((#\<) #\>)   ((#\>) #\<)
    (else ch)))

(define (make-pack-token c)
  (list '*token* c))

(define the-eof-object (with-input-from-string "" read-char))

(define (skip-pack-comments)
  (let loop ((c (peek-char)))
    (if (eof-object? c)
      #f
      (case c
        ((#\space #\tab #\newline)
         (read-char)
         (loop (peek-char)))
        ((#\#)
         (skip-until #\newline)
         (loop (peek-char)))
        (else #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; higher order pack dispatch operations

;; basic dispatch interface
(define (make-pack-dispatch fixed-len var-len? vlp packer unpacker . opt-args)
  (let ((skipper (get-optional opt-args unpacker)))
    (lambda args
      (let ((command (car args)))
        (case command
          ((pack)     (apply packer (cdr args)))
          ((unpack)   (apply unpacker (cdr args)))
          ((skip)     (apply skipper (cdr args)))
          ((packer)   packer)
          ((unpacker) unpacker)
          ((skipper)  skipper)
          ((length)   fixed-len)
          ((variable-length?)   var-len?)
          ((var-len-param)      vlp)
          ;; XXXX deprecate these?
          ((set-packer!)   (set! packer (cadr args)))
          ((set-unpacker!) (set! unpacker (cadr args)))
          ((set-skipper!)  (set! skipper (cadr args)))
          ((set-length!)   (set! fixed-len (cadr args)))
          ((set-variable-length?!) (set! var-len? (cadr args)))
          ((set-var-len-param!)    (set! vlp (cadr args)))
          ((set!)
           (let ((slot (cadr args))
                 (val (caddr args)))
             (case slot
               ((packer)   (set! packer val))
               ((unpacker) (set! unpacker val))
               ((skipper)  (set! skipper val))
               ((length)   (set! fixed-len val))
               ((variable-length?)  (set! var-len? val))
               ((var-len-param)     (set! vlp val))
               (else (error "unknown pack slot: " slot)))))
          (else (error "unknown pack command: " command)))))))

(define (copy-pack-dispatch packer)
  (make-pack-dispatch (packer 'length) (packer 'variable-length?)
                      (packer 'var-len-param) (packer 'packer)
                      (packer 'unpacker) (packer 'skipper)))

;; XXXX use this to clean up more code
(define (modify-pack-dispatch packer . args)
  (if (null? args)
    packer
    (let-optionals* args ((accessor #f)
                          (mutator identity))
      (packer 'set! accessor (mutator (packer accessor)))
      (apply modify-pack-dispatch packer (cddr args)))))

;; make a basic dispatcher for string types
(define (make-string-pack-dispatcher vlp count . opt-pad)
  (let* ((padder (if (pair? opt-pad)
                   (get-string-padder count (car opt-pad))
                   (get-string-padder count)))
         (pad (if (pair? opt-pad) (car opt-pad) #\space)))
    (make-pack-dispatch
     (if (number? count) count 0)
     (not (number? count))
     vlp
     (^v (display (padder (pop! v))) v)
     (cond
       ((equal? count #\*)
        (cut list (next-token '() (list pad '*eof*))))
       ((procedure? count)
        (let ((count-unpacker (count 'unpacker)))
          (lambda () (list (next-token-n* '() (list pad '*eof*) (get-count (count-unpacker)))))))
       (else
        (cut list (next-token-n* '() (list pad '*eof*) count))))
     (make-skipper count))))


;; set endianess of the number reader
(define (number-reader reader . opt-endian)
  (if (pair? opt-endian)
      (cute reader #f (car opt-endian))
      reader))

;; make a basic dispatcher for fixed size numeric types
(define (make-number-pack-dispatcher
         base-reader base-writer size count vlp . opt-endian)
  (let* ((splitter (get-splitter count))
         (writer (if (pair? opt-endian)
                   (number-writer base-writer (car opt-endian))
                   (number-writer base-writer)))
         (reader (if (pair? opt-endian)
                     (number-reader base-reader (car opt-endian))
                   (number-reader base-reader))))
    (cond
      ((eq? count #\*)
       (make-pack-dispatch
        0 #t vlp
        (^v (for-each writer v) '())
        (lambda ()
          (let ((res '()))
            (until (reader) eof-object? => x (push! res x))
            (reverse res)))
        (make-skipper #\*)))
      ((procedure? count)
       (let ((packer (count 'packer))
             (unpacker (count 'unpacker)))
         (make-pack-dispatch
          0 #t vlp
          (lambda (v)
            (packer (length v))
            (for-each writer v)
            '())
          (lambda ()
            (let ((res '()))
              (dotimes (i (get-count (unpacker)))
                (push! res (reader)))
              (concatenate (reverse res))))
          (lambda ()
            (let ((n (* size (get-count (unpacker)))))
              (or (port-seek (current-input-port) n SEEK_CUR)
                  (read-block n)))))))
      (else
       (make-pack-dispatch
        (* count (quotient size 8)) #f vlp
        (lambda (v)
          (let-values (((consume tail) (splitter v)))
            (for-each writer consume)
            tail))
        (lambda ()
          (let ((res '()))
            (dotimes (i count)
              (push! res (reader)))
            (reverse res)))
        (make-skipper (* count size)))))))

;; not an isolated procedure, as it doesn't handle initializing the
;; param to 0 on a pack/unpack/skip.
(define (make-count-update-pack-dispatch pack-dispatch param)
  (let ((orig-pack   (pack-dispatch 'packer))
        (orig-unpack (pack-dispatch 'unpacker))
        (orig-skip   (pack-dispatch 'skipper))
        (make-in-filter
         (lambda (p)
           (open-input-buffered-port
            (lambda (size)
              (let ((str (read-block size p)))
                (param (+ (param) (string-size str)))
                str))
            1024)))
        (make-out-filter
         (lambda (p)
           (open-output-buffered-port
            (lambda (str)
              (when str
                (let ((size (string-size str)))
                  (param (+ (param) size))
                 (display str p)
                 size)))
            1024)))
        )
    ;; make a new pack dispatch that updates the variable-length-count
    ;; parameter on io
    (make-pack-dispatch
     (pack-dispatch 'length)
     (pack-dispatch 'variable-length?)
     param
     (lambda (values)
       (with-output-to-port (make-out-filter (current-output-port))
         (lambda () (let ((v (orig-pack values))) (flush) v))))
     (lambda ()
       (with-input-from-port (make-in-filter (current-input-port))
         (lambda () (orig-unpack))))
     (lambda ()
       (with-input-from-port (make-in-filter (current-input-port))
         (lambda () (orig-skip)))) )))

;; make a dispatch that repeats a variable number of times according to
;; the slash prefix packer
(define (make-slash-repeat-pack-dispatch packer counter . opt-param)
  (let ((orig-pack    (packer  'packer))
        (orig-unpack  (packer  'unpacker))
        (orig-skip    (packer  'skipper))
        (count-pack   (counter 'packer))
        (count-unpack (counter 'unpacker))
        (param (get-optional opt-param #f)))
    (make-pack-dispatch
     (packer 'length)
     (packer 'variable-length?)
     param
     (lambda (v)
       (count-pack (list (length v)))
       (for-each (^x (orig-pack (list x))) v)
       '())
     (lambda ()
       (let ((count (get-count (count-unpack)))
             (res '()))
         (dotimes (i count)
           (push! res (orig-unpack)))
         (concatenate (reverse res))))
     (lambda ()
       (let ((count (get-count (count-unpack)))
             (res '()))
         (dotimes (i count) (orig-unpack))))
     )))

;; make a packer to repeat an arbitrary count times
(define (make-repeat-pack-dispatch packer count . opt-param)
  (let ((param (get-optional opt-param #f))
        (orig-pack (packer 'packer))
        (orig-unpack (packer 'unpacker))
        (orig-skip (packer 'skipper)))
    (cond
      ((procedure? count) (make-slash-repeat-pack-dispatch packer count param))
      ((number? count)
       (if (= count 1)
         packer
         (let ((splitter (get-splitter count)))
           (make-pack-dispatch
            (* count (packer 'length))
            (packer 'variable-length?)
            param
            (lambda (v)
              (let-values (((head tail) (splitter v)))
              (for-each (^x (orig-pack (list x))) head)
              tail))
            (lambda ()
              (let ((res '()))
                (dotimes (i count)
                  (push! res (orig-unpack)))
                (concatenate (reverse res))))
            #f))))
      ((eq? count #\*)
       (let ((splitter (get-splitter count)))
         (make-pack-dispatch
          0
          #t
          param
          (lambda (v)
            (let-values (((head tail) (splitter v)))
              (for-each (^x (orig-pack (list x))) head)
              tail))
          (lambda ()
            (let ((res '()))
              (let loop ((next (read)))
                (unless (eof-object? next)
                  (push! res next)
                  (loop (read))))
              (reverse res)))
          #f)))
      (else (error "count not supported:" count)))))

;; merges the a and b dispatchers (a followed by b)
(define (merge-pack-dispatch a b)
  (let* ((p1 (a 'packer))
         (p2 (b 'packer))
         (u1 (a 'unpacker))
         (u2 (b 'unpacker))
         (v1 (a 'variable-length?))
         (v2 (b 'variable-length?))
         (fixed? (not (or v1 v2)))
         (l1 (a 'length))
         (l2 (b 'length))
         (l (+ l1 l2))
         (s1 (a 'skipper))
         (s2 (b 'skipper)))
    (let ((p  (^v (p2 (p1 v))))
          (u  (cut append (u1) (u2)))
          (s  (if fixed? (make-skipper l) (lambda () (begin (s1) (s2))))))
      (make-pack-dispatch (+ l1 l2) (or v1 v2) (or (a 'var-len-param)
                                                   (b 'var-len-param))
                          p u s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the pack specification

;; A lot of redundancy here, this requires refactoring.  could also
;; easily make the char's dispatch on a runtime configurable hash table,
;; but that doesn't seem too useful.
(define (read-one-packer fixed-len var-len? vlp slash-count)

  (let loop ((c (read-char)))
    (cond
      ((eof-object? c)
       (make-pack-token the-eof-object))

      ;; end-of-list characters
      ((member c '(#\] #\) #\} #\>))
       (make-pack-token c))

      (else
       (let* ((bang (read-bang))   ;; optional !
              (post-count (read-count))
              (count (or slash-count post-count))
              (count-unpack (if slash-count (slash-count 'unpacker) #f)))

         (case c

           ;; ignore whitespace
           ((#\space #\tab #\newline)
            (loop (read-char)))

           ;; # indicates a line comment
           ((#\#)
            (skip-until #\newline)
            (loop (read-char)))

           ;; ( just groups until the closing )
           ((#\()
            (let* ((res (read-packers-until-token (char-complement c)
                                                  fixed-len var-len? vlp))
                   )
              res))

           ;; < produces a single list until >
           ((#\<)
            (let* ((res (read-packers-until-token (char-complement c)
                                                  fixed-len var-len? vlp))
                   (orig-pack (res 'packer))
                   (orig-unpack (res 'unpacker)))
              (res 'set-packer!
                   (^v (orig-pack (car v)) (cdr v)))
              (res 'set-unpacker!
                   (lambda () (list (orig-unpack))))
              (let ((group-count (read-count)))
                (make-repeat-pack-dispatch res group-count vlp))))

           ;; a   A string with arbitrary binary data, will be null padded.
           ((#\a)
            (let ((pad (get-string-padder count #\null)))
              (make-pack-dispatch
               (if (number? count) count 0)
               (not (number? count))
               vlp
               (^v (display (pad (pop! v))) v)
               (cond
                 ((eq? count #\*)
                  (cut list (port->string (current-input-port))))
                 ((procedure? count)
                  (lambda ()
                    (let* ((actual-count (get-count (count-unpack)))
                           (bstr (read-block actual-count))
                           (str (or (string-incomplete->complete bstr) bstr))
                           (size (string-size str)))
                      (list
                       (if (< size actual-count)
                         (string-append str (make-string (- actual-count size) #\null))
                         str)))))
                 (else
                  (lambda ()
                    (let* ((bstr (read-block count))
                           (str (or (string-incomplete->complete bstr) bstr))
                           (size (string-size str)))
                      (list
                       (if (< size count)
                         (string-append str (make-string (- count size) #\null))
                         str)))))
                 )
               (make-skipper count)
               )))

           ;; A   A text (ASCII) string, will be space padded.
           ((#\A) (make-string-pack-dispatcher vlp count))

           ;; Z   A null terminated (ASCIZ) string, will be null padded.
           ((#\Z)
            (make-pack-dispatch
             (if (number? count) count 0)
             (not (number? count))
             vlp
             (cond
               ((number? count)
                (let ((pad (get-string-padder (- count 1) #\null)))
                  (^v (display (pad (pop! v))) (display #\null))))
               (else
                (let ((pad (get-string-padder count #\null)))
                  (^v (display (pad (pop! v))) (display #\null)))))
             (cond
               ((eq? count #\*)
                (cut list (next-token '() '(#\null *eof*))))
               ((procedure? count)
                (let ((unpacker (count 'unpacker)))
                  (cut list (next-token-n* '() '(#\null *eof*) (get-count (unpacker))))))
               (else
                (cut list (next-token-n* '() '(#\null *eof*) count))))
             (make-skipper count)
             ))

           ;; b   A bit string (ascending bit order inside each byte, like vec()).
           ((#\b)
            (let ((pad (get-string-padder count #\0)))
             (make-pack-dispatch
              (if (number? count) (modulo (+ count 7) 8) 0)
              (not (number? count))
              vlp
              (lambda (v)
                (while-input-from-string (pad (pop! v))
                  (lambda ()
                    (do ((i 0 (+ i 1)) (acc 0))
                        ((= i 8) (write-byte acc))
                      ;; eof-object? and invalid bytes treated as #\0
                      (set! acc (copy-bit i acc (eq? #\1 (read-char)))) )))
                v)
              (make-port-byte-iterator
               (^i (bit-for-each (cut write <>) i)) count)
              (make-skipper count))))

           ;; B   A bit string (descending bit order inside each byte).
           ((#\B)
            (let ((pad (get-string-padder count #\0)))
              (make-pack-dispatch
               (if (number? count) (modulo (+ count 7) 8) 0)
               (not (number? count))
               vlp
               (lambda (v)
                 (while-input-from-string (pad (pop! v))
                   (lambda ()
                     (do ((i 7 (- i 1)) (acc 0))
                         ((= i -1) (write-byte acc))
                       (set! acc (copy-bit i acc (eq? #\1 (read-char)))) )))
                 v)
               (make-port-byte-iterator
                (^i (bit-reverse-for-each (cut write <>) i)) count)
               (make-skipper count))))

           ;; h   A hex string (low nybble first).
           ((#\h)
            (let ((pad (get-string-padder count #\0)))
              (make-pack-dispatch
               (if (number? count) count 0)
               (not (number? count))
               vlp
               (lambda (v)
                 (while-input-from-string (pad (pop! v))
                   (lambda ()
                     (let* ((n1 (hex-char->number (read-byte)))
                            (n2 (hex-char->number (read-byte))))
                       (write-byte (copy-bit-field n1 n2 4 8)))))
                 v)
               (make-port-byte-iterator
                (^i (format #t "~x~x" (logand i #b1111) (ash i -4))) count)
               (make-skipper count))))

           ;; H   A hex string (high nybble first).
           ((#\H)
            (let ((pad (get-string-padder count #\0)))
              (make-pack-dispatch
               (if (number? count) count 0)
               (not (number? count))
               vlp
               (lambda (v)
                 (while-input-from-string (pad (pop! v))
                   (lambda ()
                     (let* ((n1 (hex-char->number (read-byte)))
                            (n2 (hex-char->number (read-byte))))
                       (write-byte (copy-bit-field n2 n1 4 8)))))
                 v)
               (make-port-byte-iterator
                (^i (format #t "~x~x" (ash i -4) (logand i #b1111))) count)
               (make-skipper count))))

           ;; c   A signed char value.
           ((#\c)
            (make-number-pack-dispatcher read-binary-sint8 write-binary-sint8
                                         8 count vlp))

           ;; C   An unsigned char value.  Only does bytes.  See U for Unicode.
           ((#\C)
            (make-number-pack-dispatcher read-binary-uint8 write-binary-uint8
                                         8 count vlp))

           ;; s   A signed short value.
           ((#\s)
            (make-number-pack-dispatcher
             (if bang read-binary-short read-binary-sint16)
             (if bang write-binary-short write-binary-sint16)
             16 count vlp))

           ;; S   An unsigned short value.
           ((#\S)
            (make-number-pack-dispatcher
             (if bang read-binary-ushort read-binary-uint16)
             (if bang write-binary-ushort write-binary-uint16)
             16 count vlp))

           ;; i   A signed integer value.
           ;; l   A signed long value.
           ((#\i #\l)
            (make-number-pack-dispatcher
             (if bang read-binary-long read-binary-sint32)
             (if bang write-binary-long write-binary-sint32)
             32 count vlp))

           ;; I   An unsigned integer value.
           ;; L   An unsigned long value.
           ((#\I #\L)
            (make-number-pack-dispatcher
             (if bang read-binary-ulong read-binary-uint32)
             (if bang write-binary-ulong write-binary-uint32)
             32 count vlp))

           ;; n   An unsigned short in "network" (big-endian) order.
           ((#\n)
            (make-number-pack-dispatcher
             (if bang read-binary-sint16 read-binary-uint16)
             (if bang write-binary-sint16 write-binary-uint16)
             16 count vlp 'big-endian))

           ;; N   An unsigned long in "network" (big-endian) order.
           ((#\N)
            (make-number-pack-dispatcher
             (if bang read-binary-sint32 read-binary-uint32)
             (if bang write-binary-sint32 write-binary-uint32)
             32 count vlp 'big-endian))

           ;; v   An unsigned short in "VAX" (little-endian) order.
           ((#\v)
            (make-number-pack-dispatcher
             (if bang read-binary-sint16 read-binary-uint16)
             (if bang write-binary-sint16 write-binary-uint16)
             16 count vlp 'little-endian))

           ;; V   An unsigned long in "VAX" (little-endian) order.
           ((#\V)
            (make-number-pack-dispatcher
             (if bang read-binary-sint32 read-binary-uint32)
             (if bang write-binary-sint32 write-binary-uint32)
             32 count vlp 'little-endian))

           ;; q   A signed quad (64-bit) value.
           ((#\q)
            (make-number-pack-dispatcher
             read-binary-sint64 write-binary-sint64 64 count vlp))

           ;; Q   An unsigned quad value.
           ((#\Q)
            (make-number-pack-dispatcher
             read-binary-uint64 write-binary-uint64 64 count vlp))

           ;; f   A single-precision float in the native format.
           ((#\f)
            (make-number-pack-dispatcher
             read-binary-float write-binary-float 32 count vlp))

           ;; d   A double-precision float in the native format.
           ((#\d)
            (make-number-pack-dispatcher
             read-binary-double write-binary-double 64 count vlp))

           ;; w   A BER compressed integer.
           ((#\w)
            (let ((splitter (get-splitter count 0)))
              (make-pack-dispatch
               0 #t vlp
               (lambda (v)
                 (let-values (((head tail) (splitter v)))
                   (for-each (cut write-ber-integer <>) head)
                   tail))
               (if (eq? count #\*)
                 (lambda ()
                   (let ((res '()))
                     (let loop ((next (read-ber-integer)))
                       (unless (eof-object? next)
                         (push! res next)
                         (loop (read-ber-integer))))
                     (reverse res)))
                 (lambda ()
                   (let ((res '()))
                     (dotimes (i count)
                       (push! res (read-ber-integer)))
                     (reverse res))))
               (if (eq? count #\*)
                 ;; anything is a valid sequence of ber integers so we
                 ;; can safely skip to the end at this point
                 (make-skipper count)
                 (lambda ()
                   (dotimes (i count)
                     (let loop ((c (read-byte)))
                       (unless (> c 128) (loop (read-byte))))))))))

           ;; o   An sexp.
           ((#\o)
            (let ((splitter (get-splitter count 0)))
              (make-pack-dispatch
               0 #t vlp
               (lambda (v)
                 (let-values (((head tail) (splitter v)))
                   (for-each (cut write <>) head)
                   tail))
               (cond
                 ((eq? count #\*)
                  (lambda ()
                    (let ((res '()))
                      (let loop ((next (read)))
                        (unless (eof-object? next)
                          (push! res next)
                          (loop (read))))
                      (reverse res))))
                 ((procedure? count)
                  (let ((unpacker (count 'unpacker)))
                    (lambda ()
                      (let ((res '()))
                        (dotimes (i (get-count (unpacker)))
                          (push! res (read)))
                        (reverse res)))))
                 (else
                  (lambda ()
                    (let ((res '()))
                      (dotimes (i count)
                        (push! res (read)))
                      (reverse res)))))
               )))

           ;; x   A null byte.
           ((#\x)
            (let ((skipper (make-skipper count)))
              (make-pack-dispatch
               (if (number? count) count 0)
               (not (number? count))
               vlp
               (lambda (v)
                 (unless (eq? count #\*)
                   (display (make-string count #\null)))
                 v)
               ;; unpack means skip but return '() to append as nothing
               (lambda () (skipper) '())
               skipper)))

           ;; @   Null fill to absolute position.
           ((#\@)
            (let ((var-len-param (or vlp (make-parameter 0)))
                  (var-count (- count fixed-len)))
              (make-pack-dispatch
               ;;(if (number? count) count 0)
               0
               ;;(not (number? count))
               #f
               var-len-param
               (lambda (v)
                 (let ((diff (- var-count (var-len-param))))
                   (display (make-string diff #\null))
                   (var-len-param (+ (var-len-param) diff))
                   )
                 v)
               (lambda ()
                 (let ((diff (- var-count (var-len-param))))
                   (var-len-param (+ (var-len-param) diff))
                   (or (port-seek (current-input-port) diff SEEK_CUR)
                       (read-block diff))
                   '()))
               #f)))

           ;; j   A signed integer value (a Perl internal integer, IV).
           ;; J   An unsigned integer value (a Perl internal unsigned integer, UV).
           ((#\j #\J #\F #\D)
            (error "sorry, this isn't Perl, IV, UV and NV not supported"))

           (else
            (error "unknown pack template character: ~S" c)))

         )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high level parser

;; read-packer that handles / formats
(define (read-packer fixed-len var-len? vlp)
  (let ((p (read-one-packer fixed-len var-len? vlp #f)))
    (skip-pack-comments)
    (let ((c (peek-char)))
      ;; handle the / mechanism a little more generally than Perl
      (cond ((eq? c #\/)
             (read-char) ;; eat the /
             (skip-pack-comments) ;; scout ahead
             (if (memq (peek-char) '(#\a #\A #\Z #\b #\B #\h #\H))
               ;; make a string-type packer with dynamic length
               (read-one-packer fixed-len var-len? vlp p)
               ;; make an object-type dynamic repeat packer
               (make-slash-repeat-pack-dispatch
                (read-one-packer fixed-len var-len? vlp #f) p vlp)))
            (else
             ;; not a /, return the initial packer
             p)))))

(define (read-until-token tok fixed-len var-len? var-len-param)
  (let loop ((ls '()) (flen fixed-len) (vlen? var-len?) (vlp var-len-param))
    (let ((next (read-packer flen vlen? vlp)))
      (cond ((equal? next tok)
             (reverse ls))
            ((eof-object? next)
             (error "encountered eof while expecting ~S" tok))
            ((procedure? next)
             (loop (cons next ls)
                   (+ flen (next 'length))
                   (or vlen? (next 'variable-length?))
                   (or vlp (next 'var-len-param))))
            (else
             (error "packer ~S not recognized" next))))))

(define (pack-merge-folder a b)
  (if (not b)
    a
    (merge-pack-dispatch b a)))

(define (read-packers-until-token token :optional (fixed-len 0) (var-len? #f)
                                  (vlp #f))
  (let ((packers (read-until-token (make-pack-token token)
                                   fixed-len var-len? vlp)))
    ;; update var-len fields if we have an @
    ;; with make-count-update-pack-dispatch
    (let loop ((ls packers)
               (res '()))
      (if (null? ls)
        (fold pack-merge-folder #f (reverse res))
        (let ((a (car ls))
              (rest (cdr ls)))
          (if (null? rest)
            (loop rest (cons a res))
            (let ((b (car rest)))
              (if (and (not (a 'var-len-param))
                       (b 'var-len-param))
                (let ((b-vlp (b 'var-len-param)))
                  (loop rest
                        (list
                         (modify-pack-dispatch
                          (fold pack-merge-folder #f
                                (reverse
                                 (map (lambda (p)
                                        (if (p 'variable-length?)
                                          (make-count-update-pack-dispatch p (b 'var-len-param))
                                          p))
                                      (cons a res))))
                          'packer (lambda (orig) (^v (b-vlp 0) (orig v)))
                          'unpacker (lambda (orig) (lambda () (b-vlp 0) (orig)))
                          ))))
                (loop rest (cons a res))))))))))

(define (read-all-packers template)
  (with-input-from-string template
    (lambda ()
      (read-packers-until-token the-eof-object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exported interface

;; making a parameter for thread safety, maybe better shared between
;; threads with a mutex.
(define make-packer
  (let ((cache (make-parameter (make-hash-table 'equal?))))
    (lambda (template :optional (cached? #t))
      (if cached?
        (let ((res (hash-table-get (cache) template #f)))
          (unless res
            (set! res (read-all-packers template))
            (hash-table-put! (cache) template res))
          res)
        (read-all-packers template)))))

(define (pack template values :key (output #f) (to-string? #f) (cached? #t))
  (let ((packer (make-packer template cached?))
        (out (or output
                 (and to-string? (open-output-string))
                 (current-output-port))))
    (with-output-to-port out
      (lambda ()
        (let ((res (packer 'pack values)))
          (if (pair? res)
            (error "pack: extra values remaining: ~S" res)
            (if to-string?
              (get-output-string out)
              #t)))))))

(define (get-input-port keys)
  (let-keywords keys ((input #f)
                      (from-string #f))
    (or input
        (and from-string (open-input-string from-string))
        (current-input-port))))

(define (unpack template :key (cached? #t) :allow-other-keys rest)
  (let ((packer (make-packer template cached?))
        (in (get-input-port rest)))
    (with-input-from-port in
      (cut packer 'unpack))))

;; just "skip" is too vague
(define (unpack-skip template :key (cached? #t) :allow-other-keys rest)
  (let ((packer (make-packer template cached?))
        (in (get-input-port rest)))
    (with-input-from-port in
      (cut packer 'skip))))


