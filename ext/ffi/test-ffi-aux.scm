;;
;; testing gauche.ffi.ffiaux
;;

(use gauche.test)
(use file.util)
(use srfi.13)

(test-start "FFI aux")

(use gauche.ffi)
(use gauche.ffi.ffiaux)
(test-module 'gauche.ffi.ffiaux)

;;;---------------------------------------------------
(test-section "FILE*")

(define *test-file* "test-ffi-aux.o")
(define *test-content*
  (string-concatenate
   (map (^i (format "Line ~d: The quick brown fox jumps over the lazy dog.\n" i))
        (iota 100))))

(define-module ffi-aux-test-sandbox
  (use gauche.ffi)
  (with-ffi (dlopen "./ffiaux") ()
    (define-c-function read_file `(,<FILE*>) 'c-string)
    (define-c-function write_file `(,<FILE*> c-string) 'void))
  (export read_file write_file))
(import ffi-aux-test-sandbox)

(remove-files *test-file*)

(test* "read_file via port->FILE*" *test-content*
       (begin
         (string->file *test-file* *test-content*)
         (call-with-input-file *test-file*
           (^p (let* ([fp (port->FILE* p)]
                      [s (read_file fp)])
                 (close-FILE* fp)
                 s)))))

(test* "write_file via port->FILE*" *test-content*
       (begin
         (remove-files *test-file*)
         (call-with-output-file *test-file*
           (^p (let1 fp (port->FILE* p)
                 (write_file fp *test-content*)
                 (close-FILE* fp))))
         (file->string *test-file*)))

(test* "port->FILE* on non-file port"
       (test-error <error> #/port->FILE\* can only take/)
       (port->FILE* (open-input-string "abc")))

(test* "close-FILE* with wrong type" (test-error)
       (close-FILE* "foo"))

(test* "close-FILE* with null pointer"
       (test-error <error> #/null pointer/)
       (close-FILE* (null-pointer-handle)))

(remove-files *test-file*)

(test-end)
