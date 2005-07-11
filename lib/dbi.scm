;;;
;;; dbi - common database interface layer
;;;
;;;  Copyright (c) 2003-2005 Scheme Arts, L.L.C., All rights reserved.
;;;  Copyright (c) 2003-2005 Time Intermedia Corporation, All rights reserved.
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
;;;  $Id: dbi.scm,v 1.5 2005-07-11 10:15:41 shirok Exp $
;;;

;;; *EXPERIMENTAL*
;;; This module provides an abstract interface to various database
;;; management systems.  

(define-module dbi
  (use text.sxql)
  (export <dbi-error> <dbi-nonexistent-driver-error>
          <dbi-unsupported-error> <dbi-parameter-error>
          <dbi-driver> <dbi-connection> <dbi-query> <dbi-result-set>
          dbi-connect dbi-close dbi-prepare dbi-execute dbi-do
          dbi-parse-dsn dbi-make-driver dbi-prepare-sql
          ;; compatibility
          dbi-make-query dbi-execute-query dbi-get-value
          <dbi-exception>))
(select-module dbi)

;;;==============================================================
;;; DBI conditions
;;;

;; Root of dbi-related errors
(define-condition-type <dbi-error> <error> #f)

;; Failed to load the specified driver
(define-condition-type <dbi-nonexistent-driver-error> <dbi-error> #f
  ((driver-name :init-keyword :driver-name)))

;; Feature not supported
(define-condition-type <dbi-unsupported-error> <dbi-error> #f)

;; Parameter mismatch between a prepared query and its execution.
(define-condition-type <dbi-parameter-error> <dbi-error> #f)


;;;==============================================================
;;; DBI object definitions
;;;

;; <dbi-driver> is the base class of database drivers; a database
;; driver implements actual interface to a specific database system.
;; Each dbd.* module provides the concrete implementation.
;;
;; Usually the singleton instance of a concrete driver is created
;; implicitly by dbi-connect with the data-source string.  So the
;; user hardly need to see <dbi-driver> object.

(define-class <dbi-driver> ()
  ((driver-name ;:getter dbi-driver-name-of
                :init-keyword :driver-name)))

;; Base of all dbi transient objects
(define-class <dbi-object> ()
  ((open :getter dbi-open? :init-keyword :open :init-value #f)))

(define-method dbi-close ((o <dbi-object>))
  (set! (ref o 'open) #f))

;; <dbi-connection> : represents a connection to the database system.
;; All the transactions must be done while the connection is 'active',
;; or 'open'.
(define-class <dbi-connection> (<dbi-object>) ())

;; <dbi-query> : represents a query.  Query can be sent to the database
;; system by dbi-execute-query, to obtain a result set.
(define-class <dbi-query> (<dbi-object>)
  ((%prepared :init-value #f :init-keyword :prepared))
  )

;; <dbi-result-set> : an abstract entity of the result of a query.
;; For RDBMS, it is a set of rows.  The implementation may choose to
;; delay fetcing 
(define-class <dbi-result-set> (<dbi-object>) ())

;;;==============================================================
;;; User-level APIs
;;;

;; Establish a connection to the data source specified by DSN,
;; and returns a connection object.
(define (dbi-connect dsn . args)
  (receive (driver-name db-name) (dbi-parse-dsn dsn)
    (apply dbi-make-connection (dbi-make-driver driver-name) args)))

;; Prepares and returns a query object.  The default method
;; parse SQL and store it in <dbi-query>.  The driver may overload
;; this method to delegate preparation in the DBMS.
(define-method dbi-prepare ((c <dbi-connection>) (sql <string>))
  (make <dbi-query> :prepared (dbi-prepare-sql sql)))

;; Execute the prepared statement.  Again, the default method assumes
;; query is prepared by dbi module, but the driver may overload this
;; to handle execution in the DBMS.
(define-method dbi-execute ((q <dbi-query>) options . args)
  (or (and-let* ((prepared (slot-ref q '%prepared)))
        (apply prepared options args))
      (raise (condition
              (<dbi-unsupported-error>
               (message (format "dbi-execute is not implemented on ~s" q)))))
      ))

;; Does preparation and execution at once.  The driver may overload this.
(define-method dbi-do ((c <dbi-connection>) sql options . args)
  (apply (dbi-prepare-sql sql) options args))
  
;;;==============================================================
;;; DBD-level APIs
;;;

;; Subclass should implement this.
(define-method dbi-make-connection ((d <dbi-driver>) . options) #f)


;; Subclass should implement this.
(define-method dbi-get-value ((r <dbi-result-set>) (n <integer>))
  '())

;;;===================================================================
;;; Meta-utilities
;;;   Scans installed modules to find available dbd.* backends

;; Returns a list of available dbd.* backends.  Each entry is
;; a cons of a module name and its driver name.
(define (dbd-all-driver-modules)
  (library-map 'dbd.*
               (lambda (m p)
                 (cons m (path-sans-extension (sys-basename p))))))

;;;===================================================================
;;; Low-level utilities
;;;

;; Parse data source name
(define (dbi-parse-dsn data-source-name)
  (rxmatch-case data-source-name
    (#/^dbi:([\w-]+)(:[\w\/-]+)?$/ (#f driver dbname) (values driver dbname))
    (else
     (raise (condition
             (<dbi-error> (message (format "bad data source name spec: ~s"
                                           data-source-name))))))))

;; Loads a concrete driver module, and returns an instance of
;; the driver.
(define (dbi-make-driver driver-name)
  (or (and-let* ((module&path (library-fold #`"dbd/,driver-name"
                                            (lambda (m p s) (cons m p)) #f))
                 (module      (car module&path))
                 (path        (cdr module&path))
                 (class-name  (string->symbol #`"<,|driver-name|-driver>"))
                 
                 (driver-class
                  (begin (eval `(require ,path) (current-module))
                         (global-variable-ref module class-name #f)))
                 )
        (make driver-class :driver-name driver-name))
      (raise (condition
              (<dbi-nonexistent-driver-error>
               (message (format "couldn't load driver dbd.~a" driver-name))
               (driver-name driver-name))))))

;; Default prepared-SQL handler
;; dbi-prepare-sql returns a procedure, which generates a complete sql
;; when called with binding values to the parameters.
(define (dbi-prepare-sql sql)
  (let* ((tokens (sql-tokenize sql))
         (num-params (count (lambda (elt)
                              (and (pair? elt) (eq? (car elt) 'parameter)))
                            tokens)))
    (lambda (options . args)
      (unless (= (length args) num-params)
        (raise (condition
                (<dbi-parameter-error>
                 (message (format "wrong number of parameters given to an SQL ~s"
                                  sql))))))
      (call-with-output-string
        (lambda (p)
          (with-port-locking p
            (lambda ()
              (let loop ((tokens tokens)
                         (args   args)
                         (delim  #t))
                (match (car tokens)
                  ((? symbol? x)
                   (unless delim (write-char #\space p))
                   (display x p)
                   (loop (cdr tokens) args #f))
                  ((? char? x)
                   (display x p)
                   (loop (cdr tokens) args #t))
                  (('delimited x)
                   (unless delim (write-char #\space p))
                   (format p "\"~a\"" (regexp-replace-all #/\"/ x "\"\""))
                   (loop (cdr tokens) args #f))
                  (('string x)
                   (unless delim (write-char #\space p))
                   (format p "'~a'" (regexp-replace-all #/'/ x "''"))
                   (loop (cdr tokens) args #f))
                  (('number x)
                   (unless delim (write-char #\space p))
                   (display x)
                   (loop (cdr tokens) args #f))
                  (('parameter n)
                   (unless delim (write-char #\space p))
                   (display (sql-escape-literal (car args)) p)
                   (loop (cdr tokens) (cdr args) #f))
                  (('bitstring x)
                   (unless delim (write-char #\space p))
                   (format p "B'~a'" x)
                   (loop (cdr tokens) args #f))
                  (('hexstring x)
                   (unless delim (write-char #\space p))
                   (format p "X'~a'" x)
                   (loop (cdr tokens) args #f))
                  (else
                   (raise (condition
                           (<dbi-unsupported-error>
                            (message (format "unsupported SQL token ~a in ~s"
                                             (car tokens) sql))))))
                  )))))))))

;;;==============================================================
;;; Backward compatibility stuff
;;;

;; These are provided for compatibility with dbi-0.1.5 and dbd-*
;; modules that depend on it.  The newly written code shouldn't use
;; these interface.  Will be gone in a few releases.

(define <dbi-exception> <dbi-error>)

;; Older API
(define-method dbi-make-query ((c <dbi-connection>) (o <string>))
  #f)
(define-method dbi-make-query ((c <dbi-connection>))
  #f)
(define-method dbi-execute-query ((q <dbi-query>) (s <string>))
  #f)


(provide "dbi")
