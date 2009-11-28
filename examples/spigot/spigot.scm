;;;
;;; spigot - 'spigot' module Scheme wrapper
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)
;;;  I put this program in public domain.  Use it as you like.
;;;
;;;  $Id: spigot.scm,v 1.2 2003-06-12 11:09:23 shirok Exp $
;;;

;; Defines module
(define-module spigot
  (export spigot-calculate-pi
          spigot-calculate-e))
(select-module spigot)

;; Loads extension
(dynamic-load "spigot")

;; You can define Scheme functions here if you want.

