;;
;; testing gauche.logger
;;

(use gauche.test)

;; NB: logger uses gauche.fcntl.  Before 'make link', it can't be
;; loaded by 'use'.
(when (file-exists? "../ext/fcntl/fcntl.scm")
  (add-load-path "../ext/fcntl")
  (load "../ext/fcntl/fcntl"))
(when (file-exists? "../ext/syslog/syslog.scm")
  (add-load-path "../ext/syslog")
  (load "../ext/syslog/syslog"))
(test-start "logger")
(use gauche.logger)
(test-module 'gauche.logger)

;;-------------------------------------------------------------------------
(test-section "log-open")

(sys-system "rm -f test.o")

;; these shouldn't go to the log
(log-format "testing...")
(log-format "testing ~a..." 2)
(log-format "testing ~a..." 3)

(log-open "test.o")

(log-format "real testing...")
(log-format "real testing ~a..." 2)
(log-format "output string\ncontaining newline\ncharacters")

(log-open #f)

(log-format "fake testing...")

(log-open "test.o")

(log-format "real testing again...")

(test "log-open"
      '("real testing..."
        "real testing 2..."
        "output string"
        "containing newline"
        "characters"
        "real testing again...")
      (lambda ()
        (map (lambda (line)
               (cond ((#/^... .. ..:..:.. .+\[\d+\]: (.*)$/ line)
                      => (^m (m 1)))
                     (else #f)))
             (call-with-input-file "test.o" port->string-list))))

(sys-system "rm -f test.o")

;;-------------------------------------------------------------------------
(test-section "customized formatter")

(sys-system "rm -f test.o")

(log-open "test.o" :prefix "zeepa:")
(log-format "booba bunba bomba")

(test "customized formatter"
      '("zeepa:booba bunba bomba")
      (lambda ()
        (call-with-input-file "test.o" port->string-list)))

(sys-system "rm -f test.o")

(log-open "test.o" :prefix (lambda (drain) "poopa:"))
(log-format "booba bunba bomba")

(test "customized formatter"
      '("poopa:booba bunba bomba")
      (lambda ()
        (call-with-input-file "test.o" port->string-list)))

;;-------------------------------------------------------------------------
(test-section "log-from-input-port")

(sys-system "rm -f test.o")

(let-values ([(drain) (make <log-drain> :path "test.o" :prefix "log: ")]
             [(in out) (sys-pipe)])
  (log-from-input-port drain in)
  (test* "log-from-input-port"
         '("log: x (0 1 2 3 4)"
           "log: y (1 2 3 4 5)"
           "log: z (2 3 4 5 6)")
         (begin
           (format out "x ~s\n" (iota 5))
           (format out "y ~s\n" (iota 5 1))
           (format out "z ~s\n" (iota 5 2))
           ;; Need to synchronize with logger thread.
           (let loop ((retry 0))
             (when (> retry 100)
               (error "incomplete log"))
             (sys-nanosleep 1000000)
             (if (file-exists? "test.o")
               (let1 lines (call-with-input-file "test.o" port->string-list)
                 (if (< (length lines) 3)
                   (loop (+ retry 1))
                   lines))
               (loop (+ retry 1)))))))

(sys-system "rm -f test.o")

(let-values ([(drain) (make <log-drain> :path "test.o" :prefix "log: ")]
             [(in out) (sys-pipe)]
             [(eport) (open-output-string)])

  ;; parameterize to redirect warning.  needs to be here so that the
  ;; thread starts with this parameterization.
  (parameterize ((current-error-port eport))
    (log-from-input-port drain in
                         :formatter (^[line drain]
                                      (when (equal? line "b")
                                        (error "b!"))
                                      (log-display line drain))))
  (test* "log-from-input-port (error handling)"
         '(("log: a"
            "log: c")
           #t)
         (begin
           (display "a\n" out)
           (display "b\n" out)
           (display "c\n" out)
           ;; Need to synchronize with logger thread.
           (let loop ((retry 0))
             (when (> retry 100)
               (error "incomplete log"))
             (sys-nanosleep 1000000)
             (if (file-exists? "test.o")
               (let1 lines (call-with-input-file "test.o" port->string-list)
                 (if (< (length lines) 2)
                   (loop (+ retry 1))
                   (list lines
                         (boolean (#/WARNING: Error occurred during log-from-input-port formatter/
                                              (get-output-string eport))))))
               (loop (+ retry 1)))))))

(test-end)
