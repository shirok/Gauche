;;;
;;; SRFI-0   feature based conditional expansion construct
;;;
;;; $Id: srfi-0.scm,v 1.8 2001-09-17 01:41:05 shirok Exp $
;;;

;; This implementation is based on the reference implementation
;; by Mark Feeley.

(define-syntax cond-expand
  (syntax-rules (and or not else
                 srfi-0 srfi-1 srfi-2 srfi-4 srfi-6 srfi-8 srfi-9
                 srfi-11 srfi-13 srfi-14 srfi-17 srfi-23)
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
     (begin (use srfi-1) body ...))
    ((cond-expand (srfi-2 body ...) more-clauses ...)
     (begin (use srfi-2) body ...))
    ((cond-expand (srfi-4 body ...) more-clauses ...)
     (begin (use srfi-4) body ...))
    ((cond-expand (srfi-6 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-8 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-9 body ...) more-clauses ...)
     (begin (use srfi-9) body ...))
    ((cond-expand (srfi-10 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-11 body ...) more-clauses ...)
     (begin (use srfi-11) body ...))
    ((cond-expand (srfi-13 body ...) more-clauses ...)
     (begin (use srfi-13) body ...))
    ((cond-expand (srfi-14 body ...) more-clauses ...)
     (begin (use srfi-14) body ...))
    ((cond-expand (srfi-17 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-23 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
     (cond-expand more-clauses ...))))

(provide "srfi-0")
