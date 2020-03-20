(define-module gauche.experimental.command-lines
  (use file.util)
  (export os-executable-file os-command-line script-file script-directory
          command-name command-args command-line command-line-offset
          filename->command-name))

(select-module gauche.experimental.command-lines)

;;; Fundamental

(define (os-executable-file) #f)
(define (os-command-line) *os-command-line*)
(define (command-line-offset) *command-line-offset*)

;;; Derived

(define (script-directory)
  (let ((sf (script-file)))
    (and sf (string-append (sys-dirname sf) "/"))))

(define (command-line)
  (and (command-line-offset)
       (list-tail (os-command-line) (command-line-offset))))

(define (command-name)
  (and (command-line-offset)
       (filename->command-name
        (list-ref (os-command-line) (command-line-offset)))))

(define (command-args)
  (if (not (command-line-offset)) '()
      (list-tail (os-command-line) (+ (command-line-offset) 1))))

(define (command-line) (cons (or (command-name) "") (command-args)))

(define (filename->command-name arg)
  (path-sans-extension (sys-basename arg)))
