;;;
;;; SRFI-0   feature based conditional expansion construct
;;;
;;; $Id: srfi-0.scm,v 1.1 2001-03-01 20:04:02 shiro Exp $
;;;

;; This implementation is based on the reference implementation
;; by Mark Feeley.

(define-syntax cond-expand
  (syntax-rules (and or not else srfi-0 srfi-5)
    ((cond-expand) (error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
      (req1
       (cond-expand
        ((and req2 ...) body ...)
        more-clauses ...))
      more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
      (req1
       (begin body ...))
      (else
       (cond-expand
        ((or req2 ...) body ...)
        more-clauses ...))))
    ((cond-expand ((not req) body ...) more-clauses ...)
     (cond-expand
      (req
       (cond-expand more-clauses ...))
      (else body ...)))
    ((cond-expand (srfi-0 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-1 body ...) more-clauses ...)
     (begin (require "srfi-1") body ...))
    ((cond-expand (srfi-2 body ...) more-clauses ...)
     (begin (require "srfi-2") body ...))
    ((cond-expand (srfi-6 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-8 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
     (cond-expand more-clauses ...))))

(provide "srfi-0")
