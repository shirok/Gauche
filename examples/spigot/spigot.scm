;;;
;;; spigot - 'spigot' extension module example
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)
;;;  I put this program in public domain.  Use it as you like.
;;;

(in-module math.spigot)

;;
;; The 'define-cproc' forms exposes C functions to Scheme world.
;;

(inline-stub
 (.include "spigot.h")
 (define-cproc spigot-calculate-pi (digits::<int>) Spigot_calculate_pi)
 (define-cproc spigot-calculate-e (digits::<int>) Spigot_calculate_e)
 )

;; You can define Scheme functions here if you want.
