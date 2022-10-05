;;
;; Contrived example to show the source info propagation with macro
;; expansion.
;;

(use util.match)

;; (cxr a r obj) == (car obj)
;; (cxr a a r obj) == (caar obj)
;; (cxr a d a r obj) == (cadar obj)
;;etc.
(define-syntax cxr
  (syntax-rules (a d r)
    [(_ r obj) obj]
    [(_ a xs ...) (car (cxr xs ...))]
    [(_ d xs ...) (cdr (cxr xs ...))]
    [(_ . xs) (syntax-error "Malformed cxr:" (cxr . xs))]))

#|
;; 0.9.12
gosh$ (cxr a a a a r '(1 2 3 4))
*** ERROR: pair required, but got 1
Stack Trace:
_______________________________________
  0  (car (cxr a r '(1 2 3 4)))
        [unknown location]
  1  (eval expr env)
        at "/usr/share/gauche-0.98/0.9.12/lib/gauche/interactive.scm":336

;; 0.9.13
gosh$ (cxr a a a a r '(1 2 3 4))
*** ERROR: pair required, but got 1
Stack Trace:
_______________________________________
  0  (car (cxr a r '(1 2 3 4)))
        at "/home/shiro/src/Gauche/test/macro-source-info.scm":6
        expanded from (cxr a a r '(1 2 3 4))
        at "/home/shiro/src/Gauche/test/macro-source-info.scm":6
        expanded from (cxr a a a r '(1 2 3 4))
        at "/home/shiro/src/Gauche/test/macro-source-info.scm":6
        expanded from (cxr a a a a r '(1 2 3 4))
        at "(standard input)":34
  1  (eval expr env)
        at "/home/shiro/src/Gauche/src/../lib/gauche/interactive.scm":354
|#

;; (c*r aa obj) == print 'aa' and return (caar obj)
;; (c*r addar obj) == print 'addar' and return (caadr obj)
;; etc.
(define-syntax c*r
  (er-macro-transformer
   (^[form rename cmp]
     (match form
       [(_ xs obj)
        (let1 cs (map ($ string->symbol $ string $)
                      (string->list (symbol->string xs)))
          (quasirename rename
            `(begin
               (print ',xs)
               (cxr ,@cs r ,obj))))]))))


#|
;; 0.9.12
gosh$ (c*r aad '(1 2 3 4))
aad
*** ERROR: pair required, but got 2
Stack Trace:
_______________________________________
  0  (car (cxr a d r '(1 2 3 4)))
        [unknown location]
  1  (eval expr env)
        at "/usr/share/gauche-0.98/0.9.12/lib/gauche/interactive.scm":336

;; 0.9.13
gosh$ (c*r aad '(1 2 3 4))
aad
*** ERROR: pair required, but got 2
Stack Trace:
_______________________________________
  0  (car (cxr a d r '(1 2 3 4)))
        at "/home/shiro/src/Gauche/test/macro-source-info.scm":6
        expanded from (cxr a a d r '(1 2 3 4))
        at "/home/shiro/src/Gauche/test/macro-source-info.scm":47
        expanded from (c*r aad '(1 2 3 4))
        at "(standard input)":43
  1  (eval expr env)
        at "/home/shiro/src/Gauche/src/../lib/gauche/interactive.scm":354
|#
