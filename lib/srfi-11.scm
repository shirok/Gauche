;;
;; SRFI-11: Syntax for receiving multiple values
;;
;;  This implementation is based on the reference implemenetation shown
;;  in srfi-11 document <http://srfi.schemers.org/srfi-11/srfi-11.html>
;;  by Lars T Hansen.
;;
;;  Copyright (C) Lars T Hansen (1999). All Rights Reserved.
;;
;;  This document and translations of it may be copied and furnished to
;;  others, and derivative works that comment on or otherwise explain it
;;  or assist in its implementation may be prepared, copied, published and
;;  distributed, in whole or in part, without restriction of any kind,
;;  provided that the above copyright notice and this paragraph are
;;  included on all such copies and derivative works. However, this
;;  document itself may not be modified in any way, such as by removing
;;  the copyright notice or references to the Scheme Request For
;;  Implementation process or editors, except as needed for the purpose of
;;  developing SRFIs in which case the procedures for copyrights defined
;;  in the SRFI process must be followed, or as required to translate it
;;  into languages other than English.
;;
;;  The limited permissions granted above are perpetual and will not be
;;  revoked by the authors or their successors or assigns.
;;
;;  This document and the information contained herein is provided on an
;;  "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL
;;  WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;;  WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;;  RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A
;;  PARTICULAR PURPOSE.
;;

;; Adapted to Gauche by Shiro Kawai (shiro@acm.org)
;;  - Gauche prefers receive to call-with-values
;;  - Added a call to provide, so that this file can be "require"d.
;;  - Added module stuff.

(define-module srfi-11)
(select-module srfi-11)
(export let-values let*-values)

(define-syntax let-values
  (syntax-rules ()
    ((let-values (?binding ...) ?body0 ?body1 ...)
     (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))

    ((let-values "bind" () ?tmps ?body)
     (let ?tmps ?body))

    ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
     (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))

    ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
     (receive ?args ?e0
        (let-values "bind" ?bindings ?tmps ?body)))

    ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))

    ;; NB: this clause shouldn't be necessary, but Gauche's macro expander
    ;; as of 0.4.10 doesn't handle the case well.
    ((let-values "mktmp" ?a ?e0 () ?bindings (?tmp ...) ?body)
     (receive x ?e0
       (let-values "bind" ?bindings (?tmp ... (?a x)) ?body)))

    ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (receive (?arg ... . x) ?e0
       (let-values "bind" ?bindings (?tmp ... (?a x)) ?body)))
    ))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () ?body0 ?body1 ...)
     (begin ?body0 ?body1 ...))

    ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
     (let-values (?binding0)
       (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

