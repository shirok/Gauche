;;;
;;; file/filter.scm - utility to build filter programs
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
  (use srfi-13)
  (use file.util)
  (export file-filter file-filter-fold file-filter-map file-filter-for-each))
(select-module file.filter)

(define (file-filter proc :key
                     (input (current-input-port))
                     (output (current-output-port))
                     (temporary-file #f)
                     (keep-output? #f)
                     (leave-unchanged #f))

  (define (process-with-output oport)
    (cond
     [(input-port? input) (proc input oport)]
     [(string? input) (call-with-input-file input (cut proc <> oport))]
     [else (error "input must be either an input port or a file name, but got"
                  input)]))

  (define (process-with-tempfile ofile tmpf)
    (let1 tempfile (cond [(string-prefix? "/" tmpf) tmpf]
                         [(string-prefix? "./" tmpf) tmpf]
                         [(string-prefix? "../" tmpf) tmpf]
                         [else (build-path (sys-dirname ofile) tmpf)])
      (receive (tport tfile) (sys-mkstemp tempfile)
        (guard (e [else (unless keep-output? (sys-unlink tfile))
                        (raise e)])
          (begin0 (process-with-output tport)
            (close-output-port tport)
            (if (rename-ok? tfile ofile)
              (sys-rename tfile ofile)
              (sys-unlink tfile)))))))

  (define (rename-ok? tfile ofile)
    (or (not leave-unchanged)
        (not (file-is-readable? ofile))
        (and (file-is-readable? ofile)
             (file-is-readable? tfile)
             (not (file-equal? tfile ofile)))))

  (cond
   [(output-port? output) (process-with-output output)]
   [(string? output)
    (cond [(string? temporary-file)
           (process-with-tempfile output temporary-file)]
          [(eq? temporary-file #t)
           (process-with-tempfile output #"~(sys-basename output).tmp")]
          [(not temporary-file)
           (guard (e [else (unless keep-output? (sys-unlink output))
                           (raise e)])
             (call-with-output-file output process-with-output))]
          [else (error "temporary-file must be a boolean or a string, but got"
                       temporary-file)])]
   [else (error "output must be either an output port or a file name, but got"
                output)]))


(define (file-filter-fold proc seed
                          :key (reader read-line) :allow-other-keys keys)
  (apply file-filter
         (^[in out] (fold (^[elt seed] (proc elt seed out))
                          seed (generator->lseq (cut reader in))))
         keys))

(define (file-filter-map proc :key (reader read-line) :allow-other-keys keys)
  (apply file-filter
         (^[in out] (map (^[elt] (proc elt out))
                         (generator->lseq (cut reader in))))
         keys))

(define (file-filter-for-each proc
                              :key (reader read-line) :allow-other-keys keys)
  (apply file-filter
         (^[in out] (generator-for-each (cut proc <> out) (cut reader in)))
         keys))
  
         
  

