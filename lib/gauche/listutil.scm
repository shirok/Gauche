;;;
;;; auxiliary list utilities.  to be autoloaded.
;;;
;;;  Copyright(C) 2002-2003 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: listutil.scm,v 1.2 2003-01-08 09:35:21 shirok Exp $
;;;

(define-module gauche.listutil
  (export list-of cond-cons cond-append cond-append!))
(select-module gauche.listutil)

;; Cxxxr and cxxxxr are less frequently used, thus stripped out
;; from the core.  Having these defined in Scheme won't be much
;; performance loss, for cxr and cxxr in their body are both inlined.

(define-syntax %define-cxr
  (syntax-rules ()
    ((_ name a b)
     (define-in-module scheme name
       (getter-with-setter
        (lambda (x) (a (b x)))
        (lambda (x v) (set! (a (b x)) v)))))))

(%define-cxr caaar  car  caar)
(%define-cxr caadr  car  cadr)
(%define-cxr cadar  car  cdar)
(%define-cxr caddr  car  cddr)
(%define-cxr cdaar  cdr  caar)
(%define-cxr cdadr  cdr  cadr)
(%define-cxr cddar  cdr  cdar)
(%define-cxr cdddr  cdr  cddr)
(%define-cxr caaaar caar caar)
(%define-cxr caaadr caar cadr)
(%define-cxr caadar caar cdar)
(%define-cxr caaddr caar cddr)
(%define-cxr cadaar cadr caar)
(%define-cxr cadadr cadr cadr)
(%define-cxr caddar cadr cdar)
(%define-cxr cadddr cadr cddr)
(%define-cxr cdaaar cdar caar)
(%define-cxr cdaadr cdar cadr)
(%define-cxr cdadar cdar cdar)
(%define-cxr cdaddr cdar cddr)
(%define-cxr cddaar cddr caar)
(%define-cxr cddadr cddr cadr)
(%define-cxr cdddar cddr cdar)
(%define-cxr cddddr cddr cddr)

(provide "gauche/listutil")
