;;;
;;; extract - filter bilingual texinfo document
;;;
;;;  Copyright(C) 2001-2003 by Shiro Kawai (shiro@acm.org)
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

(use gauche.parseopt)
(use gauche.charconv)
(use gauche.parameter)
(use srfi-13)
(use file.util)

(define *version*
  (string-trim-both (file->string (or (find (cut sys-access <> R_OK)
                                            '("../VERSION"
                                              "./VERSION"))
                                      (error "No VERSION file?")))))
(define *node-table* '())
(define *header-table* '())

(define lang   (make-parameter 'en))
(define srcdir (make-parameter ""))

(define (scan-nodes)
  (let ([current-node #f]
        [current-header #f])
    (port-for-each
     (lambda (line)
       (rxmatch-case line
         [#/^@node\s+([^,]+)/ (#f node)
          (set! current-node (string-trim-right node))]
         [#/^@(chapter|(sub)*section|appendix\w*)\s+(.*)/ (#f #f #f header)
          (set! current-header (string-trim-right header))]
         [#/^@c NODE\s+([^,]*)(,(.*))?/ (#f jnode #f jheader)
          (let* ([jn (string-trim-right jnode)]
                 [jh (if jheader (string-trim-both jheader) jn)])
            (push! *node-table* (cons current-node jn))
            (push! *header-table* (cons current-header jh)))]
         [#/^@include\s+(\S+)/ (#f file)
           (with-input-from-file (find-included-file file)
             (cut scan-nodes) :encoding 'utf8)]
         ))
     read-line)))

(define (filter pattern-in pattern-out)
  (define (in line)
    (rxmatch-case line
      [test eof-object?]
      [pattern-in () (in (read-line))]
      [pattern-out () (out (read-line))]
      [#/^@include\s+(\S+)/ (#f file)
        (with-input-from-file (find-included-file file)
          (cut filter pattern-in pattern-out) :encoding 'utf8)
        (in (read-line))]
      ;; Extract-time include.  @include will be tracked by
      ;; texinfo-multiple-files-update and the includee must have @node
      ;; at the toplevel, so this can be used to include files without
      ;; the restriction.
      [#/^@c xinclude\s+(\S+)/ (#f file)
        (with-input-from-file (find-included-file file)
          (cut filter pattern-in pattern-out) :encoding 'utf8)
        (in (read-line))]
      [#/^@c COMMON$/ () (in (read-line))]
      [test (^_ (eq? (lang) 'en))
            (display (regexp-replace-all #/@VERSION@/ line *version*))
            (newline) (in (read-line))]
      [#/^@node\s+(.*)$/ (#f nodedesc)
        (process-node nodedesc) (in (read-line))]
      [#/^@(chapter|(sub)*section|appendix\w*)\s+(.*)/ (#f cmd #f header)
        (process-header cmd header) (in (read-line))]
      [#/^\* ([^:]+)::(.*)?/ (#f node desc)
        (process-menu node #f desc) (in (read-line))]
      [#/^\* ([^:]+):\s+([^\)]+\))\.(.*)?/ (#f tag node desc)
        (process-menu node tag desc) (in (read-line))]
      [else (display
             ($ regexp-replace-all* line
                #/@VERSION@/ *version*
                #/(@(?:px|x)?ref)\{([^\}]+)\}/ process-ref))
            (newline)
            (in (read-line))]))

  (define (out line)
    (rxmatch-case line
      [test eof-object?]
      [pattern-in ()  (in (read-line))]
      [#/^@c COMMON$/ () (in (read-line))]
      [else (out (read-line))]))

  (in (read-line)))

;; We search relative to the current directory first, then
;; relative to $srcdir; for FILE may be the generated one.
(define (find-included-file file)
  (if (file-exists? file)
    file
    (build-path (srcdir) file)))

(define (lookup name table)
  (cond [(assoc (string-trim-both name) table) => cdr]
        [else name]))

(define (process-node nodedesc)
  (display "@node ")
  (display (string-join
            (map (cut lookup <> *node-table*) (string-split nodedesc #\,))
            ", "))
  (newline))

(define (process-header cmd header)
  (format #t "@~a ~a\n" cmd (lookup header *header-table*)))

(define (process-menu node tag desc)
  (if tag
    (format #t "* ~a: ~a.  ~a\n" tag (lookup node *node-table*)
            (string-trim-both (or desc "")))
    (format #t "* ~a::  ~a\n" (lookup node *node-table*)
            (string-trim-both (or desc "")))))

(define (process-ref match)
  (let ([cmd  (rxmatch-substring match 1)]
        [node (rxmatch-substring match 2)])
    (format #f "~a{~a}" cmd (lookup node *node-table*))))

(define (usage)
  (display "Usage: extract [-en|-jp][-o outfile] infile\n")
  (exit 1))

(define (main args)
  (let ([outfile #f]
        [language 'en])
    (let1 a (parse-options (cdr args)
              (["o=s" (file) (set! outfile file)]
               ["en"  () (set! language 'en)]
               ["jp"  () (set! language 'jp)]
               [else _ (usage)]))

      (define (do-it)
        (case language
          [(en) (filter #/^@c EN$/ #/^@c JP$/)]
          [(jp) (filter #/^@c JP$/ #/^@c EN$/)]))

      (unless (= (length a) 1) (usage))

      (parameterize ([srcdir (sys-normalize-pathname (sys-dirname (car a))
                                                     :absolute #t)]
                     [lang language])
        (when (eq? language 'jp)
          (with-input-from-file (car a) scan-nodes :encoding 'utf8))
        (with-input-from-file (car a)
          (^[]
            (if outfile
              (with-output-to-file outfile do-it :encoding 'utf8)
              (let1 out (wrap-with-output-conversion
                         (current-output-port) 'utf8)
                (with-output-to-port out do-it)
                (close-output-port out))))
          :encoding 'utf8))
      0)))

;; Local variables:
;; mode: Scheme
;; end:
