;;;
;;; spigot - 'spigot' module Scheme wrapper
;;;
;;;  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: spigot.scm,v 1.1 2003-06-10 20:59:45 shirok Exp $
;;;

;; Defines module
(define-module spigot
  (export spigot-calculate-pi
          spigot-calculate-e))
(select-module spigot)

;; Loads extension
(dynamic-load "spigot")

;; You can define Scheme functions here if you want.

;; Epilogue
(provide "spigot")
