;;;
;;; srfi-13/pred - string library (predicates)
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: internal.scm,v 1.2 2001-05-01 06:44:01 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

;; Low-level procedures.  These are included for completeness, but
;; I'm not using these in other SRFI-13 routines, since it is more
;; efficient to let %maybe-substring handle argument checking as well.

(define (string-parse-start+end proc s args)
  (check-arg string? s)
  (let ((slen (string-length s)))
    (let-optional* args (start end)
      (if (undefined? start)
          (values '() 0 slen)
          (if (not (and (integer? start) (exact? start) (<= 0 start slen)))
              (error "~s: argument out of range: ~s" proc start)
              (if (undefined? end)
                  (values '() start slen)
                  (if (not (and (integer? end) (exact? end) (<= start end slen)))
                      (error "~s: argument out of range: ~s" proc end)
                      (values (cddr args) start end))
                  )
              )
          )
      )))

(define (string-parse-final-start+end proc s args)
  (check-arg string? s)
  (let ((slen (string-length s)))
    (let-optional* args (start end)
      (if (undefined? start)
          (values '() 0 slen)
          (if (not (and (integer? start) (exact? start) (<= 0 start slen)))
              (error "~s: argument out of range: ~s" proc start)
              (if (undefined? end)
                  (values '() start slen)
                  (if (not (and (integer? end) (exact? end) (<= start end slen)))
                      (error "~s: argument out of range: ~s" proc end)
                      (if (not (null? (cddr args)))
                          (error "~s: too many arguments" proc)
                          (values (cddr args) start end)))
                  )
              )
          )
      )))

(define-syntax let-string-start+end
  (syntax-rules ()
    ((_ (?start ?end ?rest) ?proc ?s ?args . ?body)
     (call-with-values
      (lambda () (string-parse-start+end ?proc ?s ?args))
      (lambda (?rest ?start ?end) . ?body)))
    ((_ (?start ?end) ?proc ?s ?args . ?body)
     (call-with-values
      (lambda () (string-parse-final-start+end ?proc ?s ?args))
      (lambda (?start ?end) . ?body)))
    ))

(define (check-substring-spec proc s start end)
  (unless (substring-spec-ok? s start end)
    (error "~s: invalid substring spec: ~s (~s, ~s)" proc s start end)))

(define (substring-spec-ok? s start end)
  (and (string? s)
       (integer? start) (exact? start)
       (integer? end) (exact? end)
       (<= 0 start end (string-length s))))

  
    


  

