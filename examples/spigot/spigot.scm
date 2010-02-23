;;;
;;; spigot - 'spigot' module Scheme wrapper
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)
;;;  I put this program in public domain.  Use it as you like.
;;;

;; Defines module
(define-module spigot
  (export spigot-calculate-pi
          spigot-calculate-e))
(select-module spigot)

;; Loads extension
(dynamic-load "spigot")

;; You can define Scheme functions here if you want.

