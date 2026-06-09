;;;
;;;  An example of FFI
;;;

;; This is to use libuuid API through gauche.ffi.  (Note: uuid feature
;; is available via rfc.uuid module.  This is purely for eexample).

(define-module ffi-libuuid
  (use gauche.native-type)
  (use gauche.ffi)
  (export uuid_t

          UUID_VARIANT_NCS
          UUID_VARIANT_DCE
          UUID_VARIANT_MICROSOFT
          UUID_VARIANT_OTHER
          UUID_VARIANT_SHIFT
          UUID_VARIANT_MASK
          UUID_TYPE_DCE_NIL
          UUID_TYPE_DCE_TIME
          UUID_TYPE_DCE_SECURITY
          UUID_TYPE_DCE_MD5
          UUID_TYPE_DCE_RANDOM
          UUID_TYPE_DCE_SHA1

          UUID_TYPE_SHIFT
          UUID_TYPE_MASK

          UUID_STR_LEN

          uuid-clear
          uuid-compare
          uuid-copy

          uuid-generate
          uuid-generate-random
          uuid-generate-time
          uuid-generate-time-safe
          uuid-generate-md5
          uuid-generate-sha1

          uuid-is-null

          uuid-parse
          uuid-parse-range

          uuid-unparse
          uuid-unparse-lower
          uuid-unparse-upper

          ;; uuid-time
          uuid-type
          uuid-variant
          uuid-get-template))
(select-module ffi-libuuid)

;; typedef unsigned char uuid_t[16];
(define uuid_t (native-type '(.array char (16))))

;; UUID Variant definitions
(define-constant UUID_VARIANT_NCS       0)
(define-constant UUID_VARIANT_DCE       1)
(define-constant UUID_VARIANT_MICROSOFT 2)
(define-constant UUID_VARIANT_OTHER     3)

(define-constant UUID_VARIANT_SHIFT     5)
(define-constant UUID_VARIANT_MASK      #x7)

;; UUID Type definitions
(define-constant UUID_TYPE_DCE_NIL    0)
(define-constant UUID_TYPE_DCE_TIME   1)
(define-constant UUID_TYPE_DCE_SECURITY 2)
(define-constant UUID_TYPE_DCE_MD5    3)
(define-constant UUID_TYPE_DCE_RANDOM 4)
(define-constant UUID_TYPE_DCE_SHA1   5)

(define-constant UUID_TYPE_SHIFT      4)
(define-constant UUID_TYPE_MASK     #xf)

(define-constant UUID_STR_LEN        37)

;; C API
(with-ffi (dynamic-load "libuuid" :init-function #f) ()
  (define-c-function uuid-clear `(,uuid_t) 'void)
  (define-c-function uuid-compare `((const ,uuid_t) (const ,uuid_t)) 'int)
  (define-c-function uuid-copy `(,uuid_t (const ,uuid_t)) 'void)

  (define-c-function uuid-generate `(,uuid_t) 'void)
  (define-c-function uuid-generate-random `(,uuid_t) 'void)
  (define-c-function uuid-generate-time `(,uuid_t) 'void)
  (define-c-function uuid-generate-time-safe `(,uuid_t) 'int)
  (define-c-function uuid-generate-md5 `(,uuid_t (const ,uuid_t) (const char*) size_t) 'void)
  (define-c-function uuid-generate-sha1 `(,uuid_t (const ,uuid_t) (const char*) size_t) 'void)

  (define-c-function uuid-is-null `((const ,uuid_t)) 'int)

  (define-c-function uuid-parse `((const char*) ,uuid_t) 'int)
  (define-c-function uuid-parse-range `((const char*) (const char*) ,uuid_t) 'int)
  (define-c-function uuid-unparse `((const ,uuid_t) char*) 'void)
  (define-c-function uuid-unparse-lower `((const ,uuid_t) char*) 'void)
  (define-c-function uuid-unparse-upper `((const ,uuid_t) char*) 'void)

  ; uuid-time
  (define-c-function uuid-type `((const ,uuid_t)) 'int)
  (define-c-function uuid-variant `((const ,uuid_t)) 'int)

  (define-c-function uuid-get-template `(c-string) uuid_t)
  )
