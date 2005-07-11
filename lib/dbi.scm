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
;;;  $Id: dbi.scm,v 1.4 2005-07-11 05:56:54 shirok Exp $
;;;

;;; *EXPERIMENTAL*
;;; This module provides an abstract interface to various database
;;; management systems.  

(define-module dbi
  (use text.sxql)
  (export <dbi-error>
          <dbi-driver> <dbi-connection> <dbi-query> <dbi-result-set>
	  dbi-make-driver dbi-make-connection dbi-make-query
	  dbi-execute-query dbi-get-value dbi-close
          ;; compatibility
          <dbi-exception))
(select-module dbi)

;;;==============================================================
;;; DBI conditions
;;;

;; Root of dbi-related errors
(define-condition-type <dbi-error> <error> #f)

;; Failed to load the specified driver
(define-condition-type <dbi-driver-not-found> <dbi-error> #f
  ((driver-name :init-keyword :driver-name)))

;; Feature not supported
(define-condition-type <dbi-fature-not-supported> <dbi-error> #f
  ((feature :init-keyword :feature)))

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
(define-class <dbi-query> (<dbi-object>) ())

;; <dbi-result-set> : an abstract entity of the result of a query.
;; For RDBMS, it is a set of rows.  The implementation may choose to
;; delay fetcing 
(define-class <dbi-result-set> (<dbi-object>) ())

;;;==============================================================
;;; User-level APIs
;;;

(define (dbi-connect dsn . args)
  (


;;;==============================================================
;;; DBD-level APIs
;;;


;; Subclass should implement this.
(define-method dbi-make-connection ((d <dbi-driver>) . options) #f)

(define-method dbi-prepare-query ((c <dbi-connection>) (sql <string>))
  #f)

(define-method dbi-execute ((q <dbi-query>) (params <list>))
  #f)

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

;; Loads a concrete driver module, and returns an instance of
;; the driver.  It is a low-level API.
(define (dbi-make-driver driver-name)
  (let ((driver-class (dbd-load-driver-class driver-name)))
    (make driver-class :driver-name driver-name)))

;; Loads the module of the given driver name, and returns the
;; driver class.  If there's no such module, returns #f.
(define (dbd-load-driver-class driver-name)
  (and-let* ((module&path (library-fold #`"dbd/,driver-name"
                                        (lambda (m p s) (cons m p)) #f))
             (class-name  (string->symbol #`"<,|driver-name|-driver>"))
             )
    (eval `(require ,(cdr module&path)) (current-module))
    (and (global-variable-bound? (car module&path) class-name)
         (eval class-name (find-module (car module&path))))))

;;;===================================================================
;;; Default prepared-SQL handler
;;;   For the drivers that doesn't handle prepared (parameterized) SQL,
;;;   we provide a default method


;;;===================================================================
;;; Low-level utilities
;;;




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
