;;
;; Experimental implementatio of planned command-line srfi
;; https://github.com/shirok/Gauche/pull/640
;;

(define-module gauche.experimental.command-lines
  (use file.util)
  (export os-executable-file os-command-line script-file script-directory
          command-name command-args command-line 
          filename->command-name

          ;; command-line-offset
          )
  )

(select-module gauche.experimental.command-lines)

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
  (and  (not (null? (command-line)))
        (filename->command-name (car (command-line)))))

(define (command-args)
  (if (null? (command-line))
    '()
    (cdr (command-line))))

(define (filename->command-name arg)
  (path-sans-extension (sys-basename arg)))
