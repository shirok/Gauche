;;;
;;; sxml.sxpath - SXML Query Language
;;;
;;;   This file is mechanically translated for Gauche from
;;;   Oleg Kiselyov's SXPATH SXML query language, SXPath.scm, v3.5.
;;;   Public domain.
;;;
;;; $Id: sxpath.scm,v 1.3 2003-07-21 20:19:19 shirok Exp $
;;;

(define-module sxml.sxpath
  (use srfi-1)
  (use srfi-13)
  (use text.parse)
  (use sxml.adaptor)
  (export sxpath
          node-parent
          node-closure
          node-or
          node-reduce
          node-join
          node-self
          node-trace
          node-reverse
          node-pos
          nodeset?
          node-typeof?
          node-eq?
          node-equal?
          select-kids
          map-union
          ))
(select-module sxml.sxpath)

;;; Generated from Oleg Kiselyov's "SXPath.scm"

(define (nodeset? x) (or (and (pair? x) (not (symbol? (car x)))) (null? x)))
(define (node-typeof? crit) (lambda (node) (case crit ((*) (and (pair? node) (not (memq (car node) '(|@| *PI*))))) ((*any*) #t) ((*text*) (string? node)) (else (and (pair? node) (eq? crit (car node)))))))
(define (node-eq? other) (lambda (node) (eq? other node)))
(define (node-equal? other) (lambda (node) (equal? other node)))
(define (node-pos n) (lambda (nodeset) (cond ((not (nodeset? nodeset)) '()) ((null? nodeset) nodeset) ((eqv? n 1) (list (car nodeset))) ((negative? n) ((node-pos (+ n 1 (length nodeset))) nodeset)) (else (assert (positive? n)) ((node-pos (|--| n)) (cdr nodeset))))))
(define (filter pred?) (lambda (lst) (let loop ((lst (if (nodeset? lst) lst (list lst))) (res '())) (if (null? lst) (reverse res) (let ((pred-result (pred? (car lst)))) (loop (cdr lst) (if (and pred-result (not (null? pred-result))) (cons (car lst) res) res)))))))
(define (take-until pred?) (lambda (lst) (let loop ((lst (if (nodeset? lst) lst (list lst)))) (if (null? lst) lst (let ((pred-result (pred? (car lst)))) (if (and pred-result (not (null? pred-result))) '() (cons (car lst) (loop (cdr lst)))))))))
(define (take-after pred?) (lambda (lst) (let loop ((lst (if (nodeset? lst) lst (list lst)))) (if (null? lst) lst (let ((pred-result (pred? (car lst)))) (if (and pred-result (not (null? pred-result))) (cdr lst) (loop (cdr lst))))))))
(define (map-union proc lst) (if (null? lst) lst (let ((proc-res (proc (car lst)))) ((if (nodeset? proc-res) append cons) proc-res (map-union proc (cdr lst))))))
(define node-reverse (lambda (node-or-nodeset) (if (not (nodeset? node-or-nodeset)) (list node-or-nodeset) (reverse node-or-nodeset))))
(define (node-trace title) (lambda (node-or-nodeset) (display "\n-->") (display title) (display " :") (pretty-print node-or-nodeset) node-or-nodeset))
(define (select-kids test-pred?) (lambda (node) (cond ((null? node) node) ((not (pair? node)) '()) ((symbol? (car node)) ((filter test-pred?) (cdr node))) (else (map-union (select-kids test-pred?) node)))))
(define node-self filter)
(define (node-join . selectors) (lambda (nodeset) (let loop ((nodeset nodeset) (selectors selectors)) (if (null? selectors) nodeset (loop (if (nodeset? nodeset) (map-union (car selectors) nodeset) ((car selectors) nodeset)) (cdr selectors))))))
(define (node-reduce . converters) (lambda (nodeset) (let loop ((nodeset nodeset) (converters converters)) (if (null? converters) nodeset (loop ((car converters) nodeset) (cdr converters))))))
(define (node-or . converters) (lambda (node-or-nodeset) (let loop ((result '()) (converters converters)) (if (null? converters) result (loop (append result (or ((car converters) node-or-nodeset) '())) (cdr converters))))))
(define (node-closure test-pred?) (lambda (node) (let loop ((parent node) (result '())) (if (null? parent) result (loop ((select-kids (node-typeof? '*)) parent) (append result ((select-kids test-pred?) parent)))))))
(define (node-parent rootnode) (lambda (node) (if (nodeset? node) (map-union (node-parent rootnode) node) (let ((pred (node-or (node-reduce (node-self (node-typeof? '*)) (select-kids (node-eq? node))) (node-join (select-kids (node-typeof? '|@|)) (select-kids (node-eq? node)))))) ((node-or (node-self pred) (node-closure pred)) rootnode)))))
(define (sxpath path) (lambda (nodeset) (let loop ((nodeset nodeset) (path path)) (cond ((null? path) nodeset) ((nodeset? nodeset) (map-union (sxpath path) nodeset)) ((procedure? (car path)) (loop ((car path) nodeset) (cdr path))) ((eq? '// (car path)) (loop ((if (nodeset? nodeset) append cons) nodeset ((node-closure (node-typeof? '*any*)) nodeset)) (cdr path))) ((symbol? (car path)) (loop ((select-kids (node-typeof? (car path))) nodeset) (cdr path))) ((and (pair? (car path)) (eq? 'equal? (caar path))) (loop ((select-kids (apply node-equal? (cdar path))) nodeset) (cdr path))) ((and (pair? (car path)) (eq? 'eq? (caar path))) (loop ((select-kids (apply node-eq? (cdar path))) nodeset) (cdr path))) ((pair? (car path)) (let reducer ((nodeset (if (symbol? (caar path)) ((select-kids (node-typeof? (caar path))) nodeset) (loop nodeset (caar path)))) (reducing-path (cdar path))) (cond ((null? reducing-path) (loop nodeset (cdr path))) ((number? (car reducing-path)) (reducer ((node-pos (car reducing-path)) nodeset) (cdr reducing-path))) (else (reducer ((filter (sxpath (car reducing-path))) nodeset) (cdr reducing-path)))))) (else (error "Invalid path step: " (car path)))))))
(define tree1 '(html (head (title "Slides")) (body (p (|@| (align "center")) (table (|@| (style "font-size: x-large")) (tr (td (|@| (align "right")) "Talks ") (td (|@| (align "center")) " = ") (td " slides + transition")) (tr (td) (td (|@| (align "center")) " = ") (td " data + control")) (tr (td) (td (|@| (align "center")) " = ") (td " programs")))) (ul (li (a (|@| (href "slides/slide0001.gif")) "Introduction")) (li (a (|@| (href "slides/slide0010.gif")) "Summary"))))))
(define tree3 '(poem (|@| (title "The Lovesong of J. Alfred Prufrock") (poet "T. S. Eliot")) (stanza (line "Let us go then, you and I,") (line "When the evening is spread out against the sky") (line "Like a patient etherized upon a table:")) (stanza (line "In the room the women come and go") (line "Talking of Michaelangelo."))))





























(provide "sxml/sxpath")

          
;; Local variables:
;; mode: scheme
;; end:
