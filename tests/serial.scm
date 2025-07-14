;;
;; test for serializer
;;

(use gauche.serializer)
(use gauche.serializer.aserializer)
(use gauche.test)

(test-start "serializer")

;; utilities
(define (topological-equal? a b)
  (let ((ctx (make-hash-table)))

    (define (tequal? a b)
      (cond ((or (boolean? a) (char? a) (number? a))
             (eqv? a b))
            ((eq? a '()) (eq? b '()))
            ((hash-table-get ctx a #f)   ;node has been appeared
             => (lambda (bb) (eq? bb b)))
            (else
             (hash-table-put! ctx a b)
             (cond ((pair? a) (and (pair? b)
                                   (tequal? (car a) (car b))
                                   (tequal? (cdr a) (cdr b))))
                   ((string? a) (and (string? b) (string=? a b)))
                   ((symbol? a) (and (symbol? b)
                                     (string=? (symbol->string a)
                                               (symbol->string b))))
                   ((keyword? a) (eqv? a b))
                   ((vector? a) (vector-tequal? a b))
                   ((is-a? a <object>) (object-equal? a b))
                   ))))

    (define (vector-tequal? a b)
      (and (vector? b)
           (let loop ((la (vector->list a)) (lb (vector->list b)))
             (cond ((null? la) (null? lb))
                   ((null? lb) #f)
                   ((tequal? (car la) (car lb)) (loop (cdr la) (cdr lb)))
                   (else #f)))))

    (define (hash-tequal? a b)
      (and (hash-table? b)
           (let loop ((la (hash-table->list a))
                      (lb (hash-table->list b)))
             (cond ((null? a) (null? b))
                   ((null? b) #f)
                   (else (let ((p (assq (caar la) lb)))
                           (and p
                                (tequal? (cdar la) (cdr p))
                                (loop (cdr la)
                                      (remove-assq (caar la) lb)))))))))
    (define (object-equal? a b)
      (and (is-a? b <object>)
           (eq? (class-of a) (class-of b))
           (let loop ((slots (get-serializable-slots a)))
             (cond ((null? slots) #t)
                   ((tequal? (slot-ref a (car slots))
                             (slot-ref b (car slots)))
                    (loop (cdr slots)))
                   (else #f)))))

    (define (remove-assq elt assoc)
      (let loop ((assoc assoc))
        (cond ((null? assoc) '())
              ((eq? elt (caar assoc)) (remove-assq elt (cdr assoc)))
              (else (cons (car assoc) (remove-assq elt (cdr assoc)))))))

    (tequal? a b)
    ))

(define (objects->string . objs)
  (call-with-output-string
   (lambda (port)
     (let ((ser (make <aserializer> :port port)))
       (for-each (lambda (item) (write-to-serializer ser item))
                 objs)))))

(define (string->objects str)
  (call-with-input-string
   str
   (lambda (port)
     (let ((ser (make <aserializer> :port port)))
       (let loop ((elt (read-from-serializer ser))
                  (data '()))
         (if (eof-object? elt)
             (reverse data)
             (loop (read-from-serializer ser)
                   (cons elt data))))))))

;;----------------------------------------------------------------------
(test-section "aserializer")

(define *primitive-types*
   '(1 -1                               ; smallint
;     999999999999999 -888888888888888   ; bignum
     3.14178 5.0e33                     ; flonum
     #f #t                              ; boolean
     #\null #\return #\A                ; char
     ()                                 ; emptylist
     "string" ""                        ; string
     x y z                              ; symbol
     :key-word                          ; keyword
     (1 2 . 3)                          ; list
     #(a b c)                           ; vector
     )
   )

(define *shared-substructure*           ; don't print this!
  (let* ((str "shared string")
         (vec '#(shared vector))
         (circ (list 1 2 3 4))
         (circt (list-tail circ 3)))
    (set-cdr! circt circ)               ;make circ circular
    (list* str vec str vec circ)))

(define-class x ()
  ((a :init-keyword :a)
   (b :init-keyword :b)
   (c :init-keyword :c))
  )

(define-class y (x)
  ((d :init-keyword :d)
   (e :init-keyword :e))
  )

(define *object-instances*
  (let* ((x1 (make x :a 0 :b 1 :c 2))
         (x2 (make x :a 3 :b 4 :c 5))
         (x3 (make x :a '(a b c) :b '(d e f) :c '(x y z)))
         (y1 (make y :a x1 :b x2 :c x3 :d #t :e #f)))
    (list x1 x2 x3 y1)))

(test "primitives" *primitive-types*
      (lambda ()
        (let* ((data *primitive-types*)
               (serialized (apply objects->string data)))
          (string->objects serialized))))

(test "shared/circular component" #t
      (lambda ()
        (let* ((data *shared-substructure*)
               (serialized (objects->string data))
               (retrieved (car (string->objects serialized))))
          ;;(display serialized)(newline)
          (topological-equal? data retrieved))))

(test "objects" #t
      (lambda ()
        (let* ((data *object-instances*)
               (serialized (write-to-string-with-serializer <aserializer> data))
               (retrieved (read-from-string-with-serializer <aserializer> serialized)))
          ;;(map describe data)
          ;;(display serialized)(newline)
          ;;(map describe retrieved)
          (topological-equal? data retrieved))))

(test "file i/o" #t
      (lambda ()
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           (let ((data (list *primitive-types*
                             *shared-substructure*
                             *object-instances*)))
             (write-to-file-with-serializer <aserializer> data "test.s")
             (topological-equal? data
                                 (read-from-file-with-serializer <aserializer>
                                                                 "test.s"))
             ))
         (lambda () (sys-remove "test.s"))
         )))

;(test "dserializer"
;      (lambda ()
;        (let* ((data *primitive-types*)
;               (serialized (write-to-string-with-serializer <dserializer> data))
;               (retrieved (read-from-string-with-serializer <dserializer> serialized))
;               )
;          ;;(display serialized)(newline)
;          (equal? data retrieved))))

(test-end)
