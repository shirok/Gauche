;;;
;;; file/filter.scm - utility to build filter programs
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: filter.scm,v 1.3 2002-10-26 09:02:41 shirok Exp $
;;;

;;; This module provides utilities for a common pattern in
;;; filter-type commands, that is, to take an input, to process
;;; the content, and to write the result.   The common occurring
;;; pattern is:
;;; 
;;; - input may be a specified file, or stdin.
;;; - output may be a specified file, or stdout.
;;; - output may be a temporary file, which will be renamed
;;;   upon completion of the processing.
;;; - output file may be removed when an error occurs in the processing.
;;;

(define-module file.filter
  (use srfi-11)
  (use srfi-13)
  (export file-filter)
  )
(select-module file.filter)

(define (file-filter proc . options)
  (let-keywords* options
      ((input  (current-input-port))
       (output (current-output-port))
       (temporary-file #f)
       (keep-output?   #f))

    (define (process-with-output oport)
      (cond
       ((input-port? input) (proc input oport))
       ((string? input)
        (call-with-input-file input (lambda (iport) (proc iport oport))))
       (else
        (error "input must be either an input port or a file name, but got"
               input))))

    (define (process-with-tempfile ofile)
      (let*-values (((tempfile) (cond ((string-prefix? "/" temporary-file)
                                       temporary-file)
                                      ((string-prefix? "./" temporary-file)
                                       temporary-file)
                                      ((string-prefix? "../" temporary-file)
                                       temporary-file)
                                      (else (string-append
                                             (sys-dirname ofile)
                                             "/"
                                             temporary-file))))
                    ((tport tfile) (sys-mkstemp tempfile)))
        (with-error-handler
         (lambda (e)
           (unless keep-output? (sys-unlink tfile))
           (raise e))
         (lambda ()
           (receive r (process-with-output tport)
             (close-output-port tport)
             (sys-rename tfile ofile)
             (apply values r))))))

    (cond
     ((output-port? output) (process-with-output output))
     ((string? output)
      (if temporary-file
          (process-with-tempfile output)
          (with-error-handler
           (lambda (e)
             (unless keep-output? (sys-unlink output))
             (raise e))
           (lambda ()
             (call-with-output-file output process-with-output)))))
     (else
      (error "output must be either an output port or a file name, but got"
             output)))
    )
  )

(provide "file/filter")




