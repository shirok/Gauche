;;;
;;; SRFI-0   feature based conditional expansion construct
;;;
;;; $Id: srfi-0.scm,v 1.12 2002-06-05 22:48:38 shirok Exp $
;;;

(define-module srfi-0
  (export cond-expand))
(select-module srfi-0)

;;; The following features are supported in all Gauche versions.
;;;
;;;   srfi-0, srfi-1, srfi-2, srfi-4, srfi-6, srfi-8,
;;;   srfi-9, srfi-11, srfi-13, srfi-14, srfi-17, srfi-19,
;;;   srfi-22, srfi-23, srfi-27, gauche
;;;

(define-syntax cond-expand
  (syntax-rules (and or not else gauche
                 srfi-0  srfi-1  srfi-2  srfi-3  srfi-4
                 srfi-5  srfi-6  srfi-8  srfi-9  srfi-10
                 srfi-11 srfi-12 srfi-13 srfi-14 srfi-15
                 srfi-16 srfi-17 srfi-18 srfi-19 srfi-20
                 srfi-21 srfi-22 srfi-23 srfi-24 srfi-25
                 srfi-26 srfi-27 srfi-28 srfi-29 srfi-30)
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
    ((cond-expand (gauche body ...) more-clauses ...)
     (begin body ...))
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
    ((cond-expand (srfi-11 body ...) more-clauses ...)
     (begin (use srfi-11) body ...))
    ((cond-expand (srfi-13 body ...) more-clauses ...)
     (begin (use srfi-13) body ...))
    ((cond-expand (srfi-14 body ...) more-clauses ...)
     (begin (use srfi-14) body ...))
    ((cond-expand (srfi-19 body ...) more-clauses ...)
     (begin (use srfi-19) body ...))
    ((cond-expand (srfi-17 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-22 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-23 body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand (srfi-27 body ...) more-clauses ...)
     (begin (use srfi-27) body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
     (cond-expand more-clauses ...))))

(provide "srfi-0")
