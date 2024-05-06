;;;
;;; spigot - 'spigot' extension module example
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)
;;;  I put this program in public domain.  Use it as you like.
;;;

;; This is a 'module definition' file.  It contains define-module
;; form, and the one that's loaded first when you say
;; (use math.spigot) in your program.
;;
;; The define-module form shows the external interface of the module---
;; what modules it depends (with 'use' form.  There's none in this example),
;; and what identifiers it exports (with 'export' form).
;;
;; Then it loads the dynamally compiled object with 'dynamic-load'.

;; Defines module
(define-module math.spigot
  (export spigot-calculate-pi
          spigot-calculate-e))
(select-module math.spigot)

(dynamic-load "math--spigot")
