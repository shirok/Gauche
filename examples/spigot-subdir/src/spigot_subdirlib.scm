;;;
;;; spigot_subdirlib.scm
;;;

;; This line ensures defined bindings are inserted into math.spigot.
(in-module math.spigot)

(inline-stub
 (.include "spigot_subdir.h")

 ;; The 'define-cproc' forms exposes C functions to Scheme world.
 ;; The following entry is a dummy one. Replace it for your definitions.

 (define-cproc test-spigot_subdir ()
   (return (test_spigot_subdir)))
 )

;; You can define Scheme procedures here, which you want to be
;; precompiled.  Note that you need to 'use' necessary modules,
;; for this file does not read the module definition file.
