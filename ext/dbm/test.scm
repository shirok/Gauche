;;
;; test for dbm package
;;

(use gauche.test)
(use srfi-1)
(use file.util)
(use gauche.collection)
(use gauche.dictionary)

(test-start "dbm")

(use dbm)
(test-module 'dbm)

;;
;; Common test suite
;;

(define-syntax catch
  (syntax-rules ()
    [(_ body ...)
     (guard (e [else #t])
       body ... #f)]))

(define *test-dbm*  "test.dbm")
(define *test2-dbm* "test2.dbm")
(define *current-dbm* #f)

;; prepare dataset
(define *test1-dataset* (make-hash-table 'equal?)) ;string only
(define *test2-dataset* (make-hash-table 'equal?)) ;other objects

(define (generate-test-set size)
  (do ((i   0 (+ i 1))
       (key (sys-random) (sys-random))
       (val (cons (sys-random) (sys-random)) (cons (sys-random) (sys-random))))
      ((>= i size))
    (hash-table-put! *test1-dataset*
                     (x->string key)
                     (x->string val))
    (hash-table-put! *test2-dataset* key val)))

(generate-test-set 1000)

;; create test
(define (test:make class rw-mode serializer)
  (set! *current-dbm*
        (dbm-open class
                  :path *test-dbm* :rw-mode rw-mode
                  :key-convert serializer
                  :value-convert serializer))
  #t)

(define (test:db-exists? class)
  (dbm-db-exists? class *test-dbm*))

;; put everything to the database
(define (test:put! dataset)
  (hash-table-for-each dataset
                       (^[k v] (dbm-put! *current-dbm* k v)))
  #t)

;; does database has all of them?
(define (test:get dataset)
  (let/cc return
    (hash-table-for-each dataset
                         (^[k v]
                           (unless (dbm-exists? *current-dbm* k)
                             (return #f))
                           (unless (equal? v (dbm-get *current-dbm* k))
                             (return #f))))
    #t))

;; does database properly deal with exceptional case?
(define (test:get-exceptional)
  (and
   ;; must raise an error
   (catch (dbm-get *current-dbm* "this_is_not_a_key"))
   ;; use default
   (dbm-get *current-dbm* "this_is_not_a_key" #t)))

;; does for-each and map do a right thing?
(define (test:for-each dataset)
  (let/cc break
    (let1 r '()
      (dbm-for-each *current-dbm*
                    (^[k v]
                      (unless (equal? v (hash-table-get dataset k #f))
                        (break #f))
                      (set! r (cons v r))))
      (equal? (reverse r) (dbm-map *current-dbm* (^[k v] v))))))

;; does collection framework work?
(define (test:collection-read dataset)
  (let/cc break
    (for-each (^[entry]
                (unless (equal? (hash-table-get dataset (car entry))
                                (cdr entry))
                  (break #f)))
              *current-dbm*)
    #t))

(define (test:collection-coerce dataset)
  (lset= equal? (hash-table->alist dataset)
         (coerce-to <list> *current-dbm*)))

;; does dictionary framework work?
(define (test:dict-for-each dataset)
  (let/cc break
    (dict-for-each *current-dbm*
                   (^[k v]
                     (unless (equal? (hash-table-get dataset k) v)
                       (break #f))))
    #t))

(define (test:dict-map dataset)
  (every (^p (equal? (hash-table-get dataset (car p)) (cdr p)))
         (dict-map *current-dbm* cons)))

(define (test:dict-keys dataset)
  (every (cut hash-table-exists? dataset <>) (dict-keys *current-dbm*)))

(define (test:dict-values dataset)
  (and (every (cute member <> (hash-table-values dataset))
              (dict-values *current-dbm*))
       #t))

;; does delete work?
(define (test:delete dataset)
  (let/cc return
    (hash-table-for-each
     dataset
     (^[k v]
       (unless (and (dbm-exists? *current-dbm* k)
                    (begin (dbm-delete! *current-dbm* k)
                           (not (dbm-exists? *current-dbm* k))))
         (return #f))))
    #t))

;; does read-only work?
(define (test:read-only)
  ;; if db is read-only, following procedures must throw an error.
  (and (catch (dbm-put! *current-dbm* "" ""))
       (catch (dbm-delete! *current-dbm* ""))))

;; does copy work?
(define (test:copy class from to)
  (dbm-db-copy class from to)
  (let ([f (dbm-open class :path from :rw-mode :read)]
        [tab (make-hash-table 'equal?)])
    (dbm-for-each f (cut hash-table-put! tab <> <>))
    (dbm-close f)
    (let1 t (dbm-open class :path to :rw-mode :read)
      (begin0
       (every (^k (equal? (dbm-get tab k) (dbm-get t k)))
              (dict-keys tab))
       (dbm-close t)))))

;; does close work?
(define (test:close)
  (dbm-close *current-dbm*)
  (and (dbm-closed? *current-dbm*)
       ;; following procedures must throw an error.
       (catch (dbm-get *current-dbm* "" #f))
       (catch (dbm-exists? *current-dbm* ""))
       (catch (dbm-put! *current-dbm* "" ""))
       (catch (dbm-delete! *current-dbm* ""))
       (catch (dbm-for-each *current-dbm* (^ _ #f)))
       (catch (dbm-map *current-dbm* (^ _ #f)))))

;; does db-remove work?
(define (test:db-remove class name)
  (and (dbm-db-exists? class name)
       (begin (dbm-db-remove class name)
              (not (dbm-db-exists? class name)))))

;; clean up files
(define (clean-up)
  (define (remover f)
    (remove-files (list f (string-append f ".dir") (string-append f ".pag")
                        (string-append f ".db"))))
  (remover *test-dbm*)
  (remover *test2-dbm*))

;; a series of test per dataset and class
(define (run-through-test class dataset serializer)
  (define (tag msg) (format #f "~s ~a" class msg))
  (dynamic-wind
   clean-up
   (^[]
     ;; create read/write db
     (test* (tag "db-exists? (pre)") #f (test:db-exists? class))
     (test* (tag "make") #t (test:make class :create serializer))
     (test* (tag "db-exists? (post)") #t (test:db-exists? class))
     ;; put stuffs
     (test* (tag "put!") #t (test:put! dataset))
     ;; get stuffs
     (test* (tag "get") #t (test:get dataset))
     (test* (tag "get-exceptional") #t (test:get-exceptional))
     ;; traverse
     (test* (tag "for-each") #t (test:for-each dataset))
     (test* (tag "collection-read") #t
            (test:collection-read dataset))
     (test* (tag "collection-coerce") #t
            (test:collection-coerce dataset))
     (test* (tag "dict-for-each") #t (test:dict-for-each dataset))
     (test* (tag "dict-map") #t (test:dict-map dataset))
     (test* (tag "dict-keys") #t (test:dict-keys dataset))
     (test* (tag "dict-values") #t (test:dict-values dataset))
     ;; close
     (test* (tag "close") #t (test:close))
     ;; open again with read only
     (test* (tag "read-only open") #t (test:make class :read serializer))
     ;; does it still have stuffs?
     (test* (tag "get again") #t (test:get dataset))
     ;; does it work as read-only?
     (test* (tag "read-only") #t (test:read-only))
     ;; close and open it again
     (test* (tag "close again") #t
            (begin
              (dbm-close *current-dbm*)
              (test:make class :write serializer)))
     ;; delete stuffs
     (test* (tag "delete") #t (test:delete dataset))
     ;; close again
     (test* (tag "close again") #t (test:close))
     ;; copy
     (test* (tag "db-copy") #t (test:copy class *test-dbm* *test2-dbm*))
     ;; remove
     (test* (tag "db-remove") #t (test:db-remove class *test2-dbm*))
     )
   clean-up))


;; Do test for two datasets
(define (full-test class)
  (test-section (format #f "~a dataset 1" (class-name class)))
  (run-through-test class *test1-dataset* #f)
  (test-section (format #f "~a dataset 2" (class-name class)))
  (run-through-test class *test2-dataset* #t)
  )

;; conditionally test
(define-macro (test-if-exists file module class)
  (when (file-exists? (string-append file "." (gauche-dso-suffix)))
    `(begin (use ,module)
            (test-module ',module)
            (full-test ,class))
    ))

;;
;; FSDBM test
;;

(use dbm.fsdbm)
(test-module 'dbm.fsdbm)
(full-test <fsdbm>)

;;
;; GDBM test
;;

(test-if-exists "dbm--gdbm" dbm.gdbm <gdbm>)

;;
;; NDBM test
;;

(test-if-exists "dbm--ndbm" dbm.ndbm <ndbm>)

;;
;; DBM test
;;

(test-if-exists "dbm--odbm" dbm.odbm <odbm>)

(test-end)
