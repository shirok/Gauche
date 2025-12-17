;;;
;;; Test foreign function interface
;;;

(use gauche.test)

(test-start "gauche.ffi")

;; bail out if we aren't configured to build ffi
;; FIXME
#;(unless (file-exists? (string-append "ffi" (gauche-dso-suffix)))
  (test-end)
  (exit 0))

(load "./ffi")
(import gauche.ffi)
(test-module 'gauche.ffi)

(test* "size-of-type int8_t" #t (number? (size-of-type 'int8)))
(test* "size-of-type uint8_t" #t (number? (size-of-type 'uint8)))
(test* "size-of-type int16_t" #t (number? (size-of-type 'int16)))
(test* "size-of-type uint16_t" #t (number? (size-of-type 'uint16)))
(test* "size-of-type int32_t" #t (number? (size-of-type 'int32)))
(test* "size-of-type uint32_t" #t (number? (size-of-type 'uint32)))
(test* "size-of-type int64_t" #t (number? (size-of-type 'int64)))
(test* "size-of-type uint64_t" #t (number? (size-of-type 'uint64)))
(test* "size-of-type char" #t (number? (size-of-type 'char)))
(test* "size-of-type unsigned-char" #t (number? (size-of-type 'unsigned-char)))
(test* "size-of-type short" #t (number? (size-of-type 'short)))
(test* "size-of-type unsigned-short" #t (number? (size-of-type 'unsigned-short)))
(test* "size-of-type int" #t (number? (size-of-type 'int)))
(test* "size-of-type unsigned-int" #t (number? (size-of-type 'unsigned-int)))
(test* "size-of-type long" #t (number? (size-of-type 'long)))
(test* "size-of-type unsigned-long" #t (number? (size-of-type 'unsigned-long)))
(test* "size-of-type float" #t (number? (size-of-type 'float)))
(test* "size-of-type double" #t (number? (size-of-type 'double)))
(test* "size-of-type pointer" #t (number? (size-of-type 'pointer)))

(test* "align-of-type int8_t" #t (number? (align-of-type 'int8)))
(test* "align-of-type uint8" #t (number? (align-of-type 'uint8)))
(test* "align-of-type int16" #t (number? (align-of-type 'int16)))
(test* "align-of-type uint16" #t (number? (align-of-type 'uint16)))
(test* "align-of-type int32" #t (number? (align-of-type 'int32)))
(test* "align-of-type uint32" #t (number? (align-of-type 'uint32)))
(test* "align-of-type int64" #t (number? (align-of-type 'int64)))
(test* "align-of-type uint64" #t (number? (align-of-type 'uint64)))
(test* "align-of-type char" #t (number? (align-of-type 'char)))
(test* "align-of-type unsigned-char" #t (number? (align-of-type 'unsigned-char)))
(test* "align-of-type short" #t (number? (align-of-type 'short)))
(test* "align-of-type unsigned-short" #t (number? (align-of-type 'unsigned-short)))
(test* "align-of-type int" #t (number? (align-of-type 'int)))
(test* "align-of-type unsigned-int" #t (number? (align-of-type 'unsigned-int)))
(test* "align-of-type long" #t (number? (align-of-type 'long)))
(test* "align-of-type unsigned-long" #t (number? (align-of-type 'unsigned-long)))
(test* "align-of-type float" #t (number? (align-of-type 'float)))
(test* "align-of-type double" #t (number? (align-of-type 'double)))
(test* "align-of-type pointer" #t (number? (align-of-type 'pointer)))

(test* "shared-object-suffix" #t (string? (shared-object-suffix)))


(define testlib (open-shared-library "./test/f"))

(define print (make-c-function testlib 'void 'print '(pointer)))

(define libc (open-shared-library "libc" '(6)))

(define test1 (make-c-function testlib 'int 'test1 '()))
(test* "make-c-function - test1" #t (= (test1) 1))

(define test2 (make-c-function testlib 'int 'test2 '()))
(test* "c-function - test2" #t (= (test2) 2))

(define plus (make-c-function testlib 'int 'plus '(int int)))
(test* "make-c-function - plus" #t (= (plus 1 1) 2))

(define string-in-string-out
  (make-c-function testlib 'pointer 'string_in_string_out '(pointer)))

(define strptr (string->pointer "Hello world"))

(test* "string->pointer/pointer?" #t (pointer? strptr))

(define ptrstr (pointer->string strptr))

(test* "pointer->string" #t (string=? ptrstr "Hello world"))

(test* "make-c-function - string-in-string-out"
       #t
       (string=? (pointer->string (string-in-string-out strptr)) "Hello world"))

(test* "allocate-pointer" #t (pointer? (allocate-pointer 32)))

(test* "null-pointer?" #t (null-pointer? null-pointer))
(define np (null-pointer))
(test* "null-pointer/null-pointer?" #t (null-pointer? np))

(close-shared-library testlib)
(close-shared-library libc)

(test-end)
