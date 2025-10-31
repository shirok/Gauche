;;;
;;; Test foreign function interface
;;;

(use gauche.test)

(test-start "gauche.ffi")

;; bail out if we aren't configured to build ffi
#;(unless (file-exists? (string-append "ffi" (gauche-dso-suffix)))
  (test-end)
  (exit 0))

(load "./ffi")
(import gauche.ffi)
(test-module 'gauche.ffi)

(test* "size-of-int8_t" #t (number? (size-of-int8_t)))
(test* "size-of-uint8_t" #t (number? (size-of-uint8_t)))
(test* "size-of-int16_t" #t (number? (size-of-int16_t)))
(test* "size-of-uint16_t" #t (number? (size-of-uint16_t)))
(test* "size-of-int32_t" #t (number? (size-of-int32_t)))
(test* "size-of-uint32_t" #t (number? (size-of-uint32_t)))
(test* "size-of-int64_t" #t (number? (size-of-int64_t)))
(test* "size-of-uint64_t" #t (number? (size-of-uint64_t)))
(test* "size-of-char" #t (number? (size-of-char)))
(test* "size-of-unsigned-char" #t (number? (size-of-unsigned-char)))
(test* "size-of-short" #t (number? (size-of-short)))
(test* "size-of-unsigned-short" #t (number? (size-of-unsigned-short)))
(test* "size-of-int" #t (number? (size-of-int)))
(test* "size-of-unsigned-int" #t (number? (size-of-unsigned-int)))
(test* "size-of-long" #t (number? (size-of-long)))
(test* "size-of-unsigned-long" #t (number? (size-of-unsigned-long)))
(test* "size-of-float" #t (number? (size-of-float)))
(test* "size-of-double" #t (number? (size-of-double)))
(test* "size-of-string" #t (number? (size-of-string)))
(test* "size-of-pointer" #t (number? (size-of-pointer)))

(test* "align-of-int8_t" #t (number? (align-of-int8_t)))
(test* "align-of-uint8" #t (number? (align-of-uint8_t)))
(test* "align-of-int16" #t (number? (align-of-int16_t)))
(test* "align-of-uint16" #t (number? (align-of-uint16_t)))
(test* "align-of-int32" #t (number? (align-of-int32_t)))
(test* "align-of-uint32" #t (number? (align-of-uint32_t)))
(test* "align-of-int64" #t (number? (align-of-int64_t)))
(test* "align-of-uint64" #t (number? (align-of-uint64_t)))
(test* "align-of-char" #t (number? (align-of-char)))
(test* "align-of-unsigned-char" #t (number? (align-of-unsigned-char)))
(test* "align-of-short" #t (number? (align-of-short)))
(test* "align-of-unsigned-short" #t (number? (align-of-unsigned-short)))
(test* "align-of-int" #t (number? (align-of-int)))
(test* "align-of-unsigned-int" #t (number? (align-of-unsigned-int)))
(test* "align-of-long" #t (number? (align-of-long)))
(test* "align-of-unsigned-long" #t (number? (align-of-unsigned-long)))
(test* "align-of-float" #t (number? (align-of-float)))
(test* "align-of-double" #t (number? (align-of-double)))
(test* "align-of-string" #t (number? (align-of-string)))
(test* "align-of-pointer" #t (number? (align-of-pointer)))

(test* "shared-object-suffix" #t (string? (shared-object-suffix)))

(define testlib (open-shared-library "./test/f"))
(define libc (open-shared-library "libc" "6"))

(define test1 (make-c-function testlib "test1" 'int '()))
(test* "make-c-function - test1" #t (= (test1) 1))

(define plus (make-c-function testlib "plus" 'int '(int int)))
(test* "make-c-function - plus" #t (= (plus 1 1) 2))

(define string-in-string-out
  (make-c-function testlib "string_in_string_out" 'pointer '(pointer)))

(define strptr (string->pointer "Hello world"))

(test* "string->pointer" #t (pointer? strptr))

(define ptrstr (pointer->string strptr))

(test* "pointer->string" #t (string=? ptrstr "Hello world"))

(test* "make-c-function - string-in-string-out"
       #t
       (string=? (pointer->string (string-in-string-out strptr)) "Hello world"))

(close-shared-library testlib)
(close-shared-library libc)

(test-end)
