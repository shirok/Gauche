;;;
;;; @@extname@@lib.scm
;;;

;; This line ensures defined bindings are inserted into @@modname@@.
(in-module @@modname@@)

(inline-stub
 (.include "@@extname@@.h")

 ;; The 'define-cproc' forms exposes C functions to Scheme world.
 ;; The following entry is a dummy one. Replace it for your definitions.

 (define-cproc test-@@extname@@ ()
   (return (test_@@extname@@)))
 )

;; You can define Scheme procedures here, which you want to be
;; precompiled.  Note that you need to 'use' necessary modules,
;; for this file does not read the module definition file.
