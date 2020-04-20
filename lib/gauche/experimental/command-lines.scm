;;
;; Experimental implementation of srfi-193
;;

(define-module gauche.experimental.command-lines
  (use srfi-13)
  (use file.util)
  (export os-executable-file os-command-line 
          command-name command-args command-line 
          script-file script-directory
          ;; filename->command-name
          )
  )
(select-module gauche.experimental.command-lines)

;; utility
(define (filename->command-name path)
  (cond-expand
   [gauche.os.windes 
    (regexp-replace #/\.(exe|scm)$/ (sys-basename path) "")]
   [else
    (regexp-replace #/\.scm$/ (sys-basename path) "")]))

;;; Fundamental

;; can be #f if we can't obtain the info
(define (os-executable-file)
  ((with-module gauche.internal %gauche-executable-path)))

;; os-command-line : Built-in
;; command-line : Built-in
;; script-file : Built-in

;;; Derived

(define (script-directory)
  (and-let1 sf (script-file)
    (string-append (sys-dirname sf) (string (path-separator)))))

(define (command-name)
  (let1 arg0 (car (command-line))
    (and (not (equal? arg0 ""))
         (filename->command-name arg0))))

(define (command-args)
  (cdr (command-line)))


