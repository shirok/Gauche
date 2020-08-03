;;
;; srfi-193 is still draft; the API may change
;;

(define-module srfi-193
  (use file.util)
  (export command-line command-name command-args
          script-file script-directory))
(select-module srfi-193)

;; utility
(define (filename->command-name path)
  (cond-expand
   [gauche.os.windes
    (regexp-replace #/\.(exe|scm)$/ (sys-basename path) "")]
   [else
    (regexp-replace #/\.scm$/ (sys-basename path) "")]))

;;; Fundamental

;; command-line : Built-in
;; script-file : Built-in

;;; Derived

(define (command-name)
  (let1 arg0 (car (command-line))
    (and (not (equal? arg0 ""))
         (filename->command-name arg0))))

(define (command-args)
  (cdr (command-line)))

(define (script-directory)
  (and-let1 sf (script-file)
    (string-append (sys-dirname sf) (string (path-separator)))))
