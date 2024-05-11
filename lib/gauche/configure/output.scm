;;;
;;; gauche.configure.output - Creating output
;;;
;;;   Copyright (c) 2013-2024  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.configure.output
  (use gauche.configure.base)
  (use gauche.dictionary)
  (use gauche.logger)
  (use gauche.package)
  (use gauche.process)
  (use file.util)
  (use file.filter)
  (use srfi.13)
  (use text.tr)
  (use util.match)
  (export cf-config-headers cf-output cf-show-substs cf-make-gpd
          cf-output-default)
  )
(select-module gauche.configure.output)

;;;
;;; Output
;;;

;; API
(define (cf-config-headers header-or-headers)
  (dolist [h (listify header-or-headers)]
    (match (string-split h #\:)
      [(out src) (push! (~(ensure-package)'config.h) (cons out src))]
      [(out) (push! (~(ensure-package)'config.h) (cons out #"~|out|.in"))]
      [_ (error "Invalid header name in cf-config-headers" h)])))

;; API
;; Like AC_OUTPUT
(define (cf-output . files)
  (define pa (ensure-package))
  (define base-substs (~ pa'substs))
  (define (make-defs)
    (if (null? (~ pa'config.h))
      (string-join
       (dict-map (~ pa'defs)
                 (^[k v]
                   (if v
                     (let1 vv ($ regexp-replace-all* (x->string v)
                                 #/\\/ "\\\\\\\\"
                                 #/\"/ "\\\\\""
                                 #/ / "\\\\ ")
                       #"-D~|k|=~|vv|")
                     "")))
       " ")
      "-DHAVE_CONFIG_H"))
  (define (make-subst path-prefix)
    (receive (srcdir top_srcdir builddir top_builddir)
        (adjust-srcdirs path-prefix)
      (let1 substs (make-stacked-map (alist->hash-table
                                      `((srcdir       . ,srcdir)
                                        (top_srcdir   . ,top_srcdir)
                                        (builddir     . ,builddir)
                                        (top_builddir . ,top_builddir)
                                        (DEFS . ,(make-defs)))
                                      'eq?)
                                     base-substs)
        (^[m]
          (let1 name (string->symbol (m 1))
            (or (dict-get substs name #f)
                (begin (warn "@~a@ isn't substituted.\n" name)
                       #"@~|name|@")))))))
  ;; We use '/' in the replaced pathname even on Windows; that's what
  ;; autoconf-generated configure does, and it's less likely to confuse
  ;; Unix-originated tools.
  (define (simplify-path+ path)
    (cond-expand
     [gauche.os.windows (string-tr (simplify-path path) "\\\\" "/")]
     [else (simplify-path path)]))
  (define (adjust-srcdirs path-prefix)
    (let ([srcdir    (~ base-substs'srcdir)]
          [tsrcdir   (~ base-substs'top_srcdir)]
          [builddir  (~ base-substs'builddir)]
          [tbuilddir (~ base-substs'top_builddir)])
      (if (equal? path-prefix ".")
        (values srcdir tsrcdir builddir tbuilddir)
        (let1 revpath ($ apply build-path
                         $ map (^_ "..") (string-split path-prefix #[\\/]))
          (values
           (cond [(equal? srcdir ".") srcdir]
                 [(absolute-path? srcdir)
                  (simplify-path+ (build-path srcdir path-prefix))]
                 [else
                  (simplify-path+ (build-path revpath srcdir path-prefix))])
           (cond [(absolute-path? tsrcdir) tsrcdir]
                 [else (simplify-path+ (build-path revpath tsrcdir))])
           (cond [(equal? builddir ".") builddir]
                 [(absolute-path? builddir)
                  (simplify-path+ (build-path builddir path-prefix))]
                 [else
                  (simplify-path+ (build-path revpath builddir path-prefix))])
           (cond [(absolute-path? tbuilddir) tbuilddir]
                 [(simplify-path+ (build-path revpath tbuilddir))]))))))

  (define (make-replace-1 output-file)
    (let1 subst (make-subst (sys-dirname (simplify-path+ output-file)))
      (^[line outp]
        (display (regexp-replace-all #/@(\w+)@/ line subst) outp)
        (newline outp))))

  (define (make-config.h)
    (^[line outp]
      (rxmatch-case line
        [#/^#undef\s+([A-Za-z_]+)/ (_ name)
         (if-let1 defval (dict-get (~ pa'defs) (string->symbol name) #f)
           (display #"#define ~name ~defval" outp)
           (display #"/* #undef ~name */" outp))]
        [else (display line outp)])
      (newline outp)))

  ;; Realize prefix and exec_prefix if they're not set.
  (when (equal? (cf$ 'prefix) "NONE")
    (cf-subst 'prefix (cf$ 'default_prefix)))
  (when (equal? (cf$ 'exec_prefix) "NONE")
    (cf-subst 'exec_prefix "${prefix}"))

  (dolist [f files]
    (let1 inf (build-path (cf$'srcdir) #"~|f|.in")
      (unless (file-is-readable? inf)
        (error "Cannot read input file ~s" inf))
      (unless (file-is-directory? (sys-dirname f))
        (make-directory* (sys-dirname f)))
      (cf-msg-notice "configure: creating ~a" f)
      (file-filter-for-each (make-replace-1 f) :input inf :output f
                            :temporary-file #t :leave-unchanged #t)))
  (dolist [h (~ pa'config.h)]
    (let1 inf (build-path (cf$'srcdir) (cdr h))
      (unless (file-is-readable? inf)
        (error "Cannot read input file ~s" inf))
      (unless (file-is-directory? (sys-dirname (car h)))
        (make-directory* (sys-dirname (car h))))
      (cf-msg-notice "configure: creating ~a" (car h))
      (file-filter-for-each (make-config.h) :input inf :output (car h)
                            :temporary-file #t :leave-unchanged #t)))

  ;; Record output variables and definitions to config.log
  (log-output-substs)
  (log-output-defs)
  )

(define (log-output-substs)
  (log-format ".")
  (log-format "## ----------------- ##")
  (log-format "## Output variables. ##")
  (log-format "## ----------------- ##")
  (log-format ".")
  (dolist [k (sort (hash-table-keys (~ (current-package)'substs)))]
    (log-format "~a=~a" k (shell-escape-string
                           (hash-table-get (~ (current-package)'substs) k)))))

(define (log-output-defs)
  (log-format ".")
  (log-format "## ------------ ##")
  (log-format "## Definitions. ##")
  (log-format "## ------------ ##")
  (log-format ".")
  (dolist [k (sort (hash-table-keys (~ (current-package)'defs)))]
    (log-format "#define ~a ~a" k
                (or (hash-table-get (~ (current-package)'defs) k) "/**/"))))

;; API
;; Show definitions.
(define (cf-show-substs :key (formatter (^[k v] (format #t "~16s ~s" k v))))
  (let1 dict (~ (ensure-package)'substs)
    (dolist [k (sort (dict-keys dict)
                     (^[a b] (string<? (x->string a) (x->string b))))]
      (formatter k (dict-get dict k))
      (newline))))

;; API
;; Create .gpd file.  This is Gauche-specific.
(define (cf-make-gpd)
  (let ([gpd-file #"~(cf$ 'PACKAGE_NAME).gpd"]
        [gpd (~ (ensure-package)'gpd)])
    (cf-echo #"creating ~gpd-file")
    (set! (~ gpd'configure)
          ($ string-join $ cons "./configure"
             $ map shell-escape-string $ cdr $ command-line))
    (with-output-to-file gpd-file
      (cut write-gauche-package-description gpd))))

;; API
;; Packages common output
(define (cf-output-default . output-files)
  (cf-make-gpd)
  (cf-echo (cf$ 'PACKAGE_VERSION) > "VERSION")
  (let* ([pfx (cf$'srcdir)]
         [outfiles (append
                    ($ map (^f (string-drop (string-drop-right f 3)
                                            (+ (string-length pfx) 1)))
                       $ glob #"~|pfx|/**/Makefile.in")
                    output-files)])
    (apply cf-output outfiles)))
