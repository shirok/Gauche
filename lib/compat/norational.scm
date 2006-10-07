;;;
;;; compat.norational - override '/' for compatibility
;;;  
;;; $Id: norational.scm,v 1.2 2006-10-07 07:38:10 shirok Exp $
;;;

(define-module compat.norational
  (export /))

(define / inexact-/)

(provide "compat/norational")

