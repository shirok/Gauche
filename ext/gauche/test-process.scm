;;
;; test gauche.process
;;

(use gauche.test)
(use srfi.13)
(test-start "gauche.process")

(use gauche.process)
(test-module 'gauche.process)

(define *nulldev*
  (cond-expand [gauche.os.windows "NUL"] [else "/dev/null"]))

;; Emulate cat, grep and ls with Gauche script, so that it works
;; on any platform.
(sys-unlink "testc.o")
(with-output-to-file "testc.o"
  (^[]
    (write '(define (main args)
              (cond [(equal? (cadr args) "cat")  (cat (cddr args))]
                    [(equal? (cadr args) "ls")   (ls (cddr args))]
                    [(equal? (cadr args) "grep") (grep (cddr args))]
                    [(equal? (cadr args) "env")  (env (cddr args))])))
    (write '(define (cat args)
              (guard (e [else
                         (format (current-error-port) "~a~%" (ref e'message))
                         1])
                (set! (port-buffering (current-input-port)) :none)
                (if (null? args)
                  (begin
                    (set! (port-buffering (current-input-port)) :none)
                    (set! (port-buffering (current-output-port)) :none)
                    (copy-port (current-input-port) (current-output-port)
                               :unit 'byte))
                  (dolist [f args]
                    (call-with-input-file f
                      (cut copy-port <> (current-output-port)))))
                0)))
    (write '(define (ls _)
              ;; glob will be tested later, so we avoid using it.
              (for-each print (sort (sys-readdir ".")))
              0))
    (write '(define (grep args)
              (let* ([opt-v (equal? (car args) "-v")]
                     [pat (string->regexp (if opt-v (cadr args) (car args)))]
                     [files (if opt-v (cddr args) (cdr args))]
                     [hit 1])
                (define (grep-1 generator)
                  (generator-for-each
                   (lambda (line)
                     (when ((if opt-v not identity)
                            (rxmatch pat line))
                       (set! hit 0)
                       (print line)))
                   generator))
                (if (null? files)
                  (grep-1 read-line)
                  (dolist [f files]
                    (with-input-from-file f
                      (cut grep-1 read-line))))
                hit)))
    (write '(define (env args)
              (for-each print (sys-environ))
              0))
    ))

;; shorthand of normalizing pathname.  this doesn't do anything on
;; unix, but on Windows separators in PATHNAME are replaced.
(define (N pathname) (sys-normalize-pathname pathname))

(define (cmd . args)
  `(,(N "../../src/gosh") "-ftest" ,(N "./testc.o") ,@args))

(define (cmd-in-subdir . args)
  `(,(N "../../../src/gosh") "-ftest",(N "../testc.o") ,@args))

(define (cmds . args)
  (let1 cmdlist (apply cmd args)
    (string-concatenate (apply append (map (^x `(,x " ")) cmdlist)))))

(define (rmrf . files)
  (dolist [f files]
    (cond-expand
     [gauche.os.windows
      (sys-system #"rmdir /q /s ~(N f) > NUL 2>&1")
      (sys-system #"del /q ~(N f) > NUL 2>&1")]
     [else
      (sys-system #"rm -rf ~f > /dev/null")])))

(define (touch file)
  (with-output-to-file file (cut values)))

;; Avoid locale specific behavior of client programs
(cond-expand
 [gauche.sys.setenv
  (sys-putenv "LANG" "C")]
 [else])

;;-------------------------------
(test-section "process object")

(rmrf "test.o" "test1.o")
(touch "test.o")

(test* "run-process (old)" 0
       (let1 p (apply run-process `(,@(cmd 'ls) :output "test.o"))
         (and (process-wait p) (process-exit-status p))))

(test* "run-process" 0
       (let1 p (run-process (cmd 'ls) :output "test.o")
         (and (process-wait p) (process-exit-status p))))

(test* "run-process (old)" 0
       (let1 p (apply run-process `(,@(cmd "grep" "test.o")
                                    :input "test.o" :output ,*nulldev*))
         (and (process-wait p) (process-exit-status p))))
(test* "run-process" 0
       (let1 p (run-process (cmd "grep" "test.o")
                            :input "test.o" :output *nulldev*)
         (and (process-wait p) (process-exit-status p))))

(test* "run-process (old)" 1
       (let1 p (apply run-process `(,@(cmd "grep" "NoSuchFile")
                                    :input "test.o" :output ,*nulldev*))
         (and (process-wait p)
              (sys-wait-exited? (process-exit-status p))
              (sys-wait-exit-status (process-exit-status p)))))

(test* "run-process" 1
       (let1 p (run-process (cmd "grep" "NoSuchFile")
                            :input "test.o" :output *nulldev*)
         (and (process-wait p)
              (sys-wait-exited? (process-exit-status p))
              (sys-wait-exit-status (process-exit-status p)))))

(test* "run-process (output pipe)" '(0 #t)
       (let* ((p  (run-process (cmd "cat" "test.o") :output :pipe))
              (in (process-output p))
              (s  (port->string in))
              (c  (call-with-input-file "test.o" port->string))
              (x  (and (process-wait p) (process-exit-status p))))
         (list x (equal? c s))))

(test* "run-process (output pipe - new format)" '(0 #t)
       (let* ((p  (run-process (cmd "cat" "test.o") :redirects '((> 1 out))))
              (in (process-output p 'out))
              (s  (port->string in))
              (c  (call-with-input-file "test.o" port->string))
              (x  (and (process-wait p) (process-exit-status p))))
         (list x (equal? c s))))

(test* "run-process (input pipe)" '(0 #t)
       (let* ((p  (run-process (cmd "cat") :input :pipe :output :pipe))
              (out (process-input p))
              (in  (process-output p))
              (s   "test\ntest"))
         (display s out)
         (flush out)
         (close-output-port out)
         (let* ((ss (port->string in))
                (x  (and (process-wait p) (process-exit-status p))))
           (list x (equal? s ss)))))

(test* "run-process (input pipe - new format)" '(0 #t)
       (let* ((p  (run-process (cmd "cat") :redirects '((< 0 in) (> 1 out))))
              (out (process-input p 'in))
              (in  (process-output p 'out))
              (s   "test\ntest"))
         (display s out)
         (flush out)
         (close-output-port out)
         (let* ((ss (port->string in))
                (x  (and (process-wait p) (process-exit-status p))))
           (list x (equal? s ss)))))

(test* "run-process (input pipe - new format 2)" '(0 #t)
       (let* ((s   "test\ntest")
              (p  (run-process (cmd "cat") :redirects `((<< 0 ,s)
                                                        (> 1 out))))
              (in  (process-output p 'out)))
         (let* ((ss (port->string in))
                (x  (and (process-wait p) (process-exit-status p))))
           (list x (equal? s ss)))))

(test* "run-process (input pipe - new format 3)" '(0 #t)
       (let* ((s0  '("test" #f test 3242))
              (s1 (write-to-string s0))
              (p  (run-process (cmd "cat") :redirects `((<<< 0 ,s0)
                                                        (> 1 out))))
              (in  (process-output p 'out)))
         (let* ((ss (port->string in))
                (x  (and (process-wait p) (process-exit-status p))))
           (list x (equal? s1 ss)))))

(test* "run-process (error pipe)" #t
       (let* ((p  (run-process (cmd "cat" "NoSuchFile") :error :pipe))
              (in (process-error p))
              (s  (port->string in))
              (x  (process-wait p))
              (p1 (run-process (cmd "cat" "NoSuchFile") :error "test.o"))
              (s1 (and (process-wait p1)
                       (call-with-input-file "test.o" port->string)))
              )
         (equal? s s1)))

;; Quirk:
;;  OSX adds __CF_USER_TEXT_ENCODING env var unconditionally.
;;  On Windows we add AVOID_EMPTY_ENVIRONMENT, for CreateProcess doesn't
;;   like an empty environment.
(test* "run-process (environment)" '()
       (let* ((p  (run-process (cmd "env") :output :pipe
                               :environment '()))
              (in (process-output p))
              (s  (port->string-list in))
              (x  (process-wait p)))
         (remove #/^(__CF_USER_TEXT_ENCODING=|AVOID_EMPTY_ENVIRONMENT=)/ s)))

(test* "run-process (environment)" '("FOO=BAR")
       (let* ((p  (run-process (cmd "env") :output :pipe
                               :environment '("FOO=BAR")))
              (in (process-output p))
              (s  (port->string-list in))
              (x  (process-wait p)))
         (remove #/^__CF_USER_TEXT_ENCODING=/ s)))

(test* "do-process :on-abnormal-exit #f" #f
       (do-process (cmd "cat" "NoSuchFile") :output :null :error :null))
(test* "do-process :on-abnormal-exit #f" #t
       (do-process (cmd "cat" "test.o") :output :null :error :null))
(test* "do-process :on-abnormal-exit :error"
       (test-error <process-abnormal-exit>)
       (do-process (cmd "cat" "NoSuchFile")
                   :on-abnormal-exit :error :output :null :error :null))
(test* "do-process :on-abnormal-exit :error"
       #t
       (do-process (cmd "cat" "test.o")
                   :on-abnormal-exit :error :output :null :error :null))
(test* "do-process :on-abnormal-exit :exit-code"
       1
       (do-process (cmd "grep" "ThereCantBeSuchLine" "test.o")
                   :on-abnormal-exit :exit-code :output :null :error :null))

;; NB: how to test :wait and :fork?

(test* "process-kill" SIGKILL
       (let ((p (run-process (cmd "cat")
                             :input :pipe :output :pipe
                             :error *nulldev*)))
         ;; handshake to make sure child process is up
         (display "abc\n" (process-input p))
         (flush (process-input p))
         (read-line (process-output p))
         (process-kill p)
         (process-wait p)
         (let ((x (process-exit-status p)))
           (and (sys-wait-signaled? x)
                (sys-wait-termsig x)))))

(test* "non-blocking wait" '(#f #t #f)
       (let* ((p  (run-process (cmd "cat")
                               :input :pipe :output :pipe
                               :error *nulldev*))
              (r0 (process-wait p #t)))
         ;; handshake to make sure child process is up
         (display "abc\n" (process-input p))
         (flush (process-input p))
         (read-line (process-output p))
         (let* ((r1 (begin (process-kill p) (process-wait p)))
                (r2 (process-wait p #t)))
           (list r0 r1 r2))))

(test* "wait with signalling error" (list #t SIGKILL)
       (guard (e ((<process-abnormal-exit> e)
                  (let ((s (process-exit-status (ref e 'process))))
                    (list (sys-wait-signaled? s)
                          (sys-wait-termsig s)))))
         (let1 p (run-process (cmd "cat")
                              :input :pipe :output :pipe
                              :error *nulldev*)
           ;; handshake to make sure child process is up
           (display "abc\n" (process-input p))
           (flush (process-input p))
           (read-line (process-output p))
           (process-kill p)
           (process-wait p #f #t))))

(test* "process-list" '()
       (process-list))

;;-------------------------------
(test-section "pipeline")

(let ()
  (with-output-to-file "test.o"
    (^[] (for-each print '("banana" "habana" "tabata" "cabara"))))

  (test* "pipelining 1" "banana\nhabana\n"
         (let1 p (run-pipeline `(,(cmd "cat" "test.o")
                                 ,(cmd "grep" "bana"))
                               :output :pipe)
           (process-wait p)
           (port->string (process-output p))))

  (test* "pipelining 2" "tabata\ncabara\n"
         (let1 p (run-pipeline `(,(cmd "cat")
                                 ,(cmd "grep" "-v" "bana"))
                               :input "test.o" :output :pipe)
           (process-wait p)
           (port->string (process-output p))))

  (test* "pipelining 3" "banana\ncabara\n"
         (let1 p (run-pipeline `(,(cmd "cat")
                                 ,(cmd "grep" "-v" "ta")
                                 ,(cmd "grep" "-v" "ha"))
                               :input "test.o" :output :pipe)
           (process-wait p)
           (port->string (process-output p))))

  (test* "pipelining 4" "habana\ntabata\ncabara\n"
         (let1 p (run-pipeline `(,(cmd "cat")
                                 ,(cmd "grep" "aba"))
                               :input :pipe :output :pipe)
           (display "banana\nhabana\ntabata\ncabara\n"
                    (process-input p))
           (close-port (process-input p))
           (process-wait p)
           (port->string (process-output p))))

  (test* "pipelining +environment" "FOO=BAR\n"
         (let1 p (run-pipeline `(,(cmd "env")
                                 ,(cmd "grep" "^FOO="))
                               :input :pipe :output :pipe
                               :environment (cons "FOO=BAR" (sys-environ)))
           (process-wait p)
           (port->string (process-output p))))
  )

;;-------------------------------
(test-section "process ports")

(rmrf "test.o" "test1.o" "test2.o")
(touch "test.o")
(sys-system (cmds "ls" "-a" ">" "test.o"))

(test* "open-input-process-port" #t
       (receive (p process) (open-input-process-port (cmd "ls" '-a))
         (let ((r (port->string p))
               (s (call-with-input-file "test.o" port->string)))
           (close-input-port p)
           (process-wait process)
           (equal? r s))))

(test* "open-input-process-port (redirect)" #t
       (receive (p process) (open-input-process-port (cmd "cat") :input "test.o")
         (let ((r (port->string p))
               (s (call-with-input-file "test.o" port->string)))
           (close-input-port p)
           (process-wait process)
           (equal? r s))))

(test* "open-input-process-port (redirect/error)" #t
       (receive (p process) (open-input-process-port (cmd "cat" "NoSuchFile")
                                                     :error "test1.o")
         (process-wait process)
         (sys-system (cmds "cat" "NoSuchFile" "2>" "test2.o"))
         (let ((r (call-with-input-file "test1.o" port->string))
               (s (call-with-input-file "test2.o" port->string)))
           (equal? r s))))

(rmrf "test1.o" "test2.o")

(test* "call-with-input-process" #t
       (let ((r (call-with-input-process (cmd "ls" '-a) port->string))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "call-with-input-process (redirect)" #t
       (let ((r (call-with-input-process (cmd "cat") port->string
                                         :input "test.o"))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "call-with-input-process (redirect/error - ignore)" #t
       (begin (call-with-input-process (cmd "cat" "NoSuchFile")
                port->string
                :error "test1.o" :on-abnormal-exit :ignore)
              (sys-system (cmds "cat" "NoSuchFile" "2>" "test2.o"))
              (let ((r (call-with-input-file "test1.o" port->string))
                    (s (call-with-input-file "test2.o" port->string)))
                (equal? r s))))

(test* "call-with-input-process (redirect/error - error)"
       (test-error <process-abnormal-exit>)
       (call-with-input-process (cmd "cat" "NoSuchFile")
         port->string :error "test1.o"))

(test* "call-with-input-process (redirect/error - #f)" #f
       (call-with-input-process (cmd "cat" "NoSuchFile")
         port->string
         :error :null :on-abnormal-exit #f))

;; NB: On Solaris, cat seems to return 2 in case the file doesn't exist.
(test* "call-with-input-process (redirect/error - handle)" (test-one-of 1 2)
       (let/cc k
         (call-with-input-process (cmd "cat" 'NoSuchFile)
           port->string
           :error "test1.o"
           :on-abnormal-exit (lambda (p)
                               (k (sys-wait-exit-status
                                   (process-exit-status p)))))))

(rmrf "test1.o" "test2.o")

(test* "with-input-from-process" #t
       (let ((r (with-input-from-process (cmd "cat" 'test.o)
                  (lambda () (port->string (current-input-port)))))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "with-input-from-process" #t
       (let ((r (with-input-from-process (cmds "cat" "<" "test.o")
                  (lambda () (port->string (current-input-port)))))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "with-input-from-process (redirect)" #t
       (let ((r (with-input-from-process (cmd "cat" 'test.o)
                  (lambda () (port->string (current-input-port)))
                  :input "test.o"))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "open-output-process-port" #t
       (let1 s (call-with-input-file "test.o" port->string)
         (rmrf "test.o")
         (receive (p process)
             (open-output-process-port (cmds "cat" ">" "test.o"))
           (display s p)
           (close-output-port p)
           (process-wait process)
           (let1 r (call-with-input-file "test.o" port->string)
             (equal? r s)))))

(test* "open-output-process-port (redirect)" #t
       (let1 s (call-with-input-file "test.o" port->string)
         (rmrf "test.o")
         (receive (p process)
             (open-output-process-port (cmd "cat") :output "test.o")
           (display s p)
           (close-output-port p)
           (process-wait process)
           (let1 r (call-with-input-file "test.o" port->string)
             (equal? r s)))))

(test* "open-output-process-port (redirect/error)" #t
       (let1 s (call-with-input-file "test.o" port->string)
         (receive (p process)
             (open-output-process-port (cmds "cat" "NoSuchFile")
                                       :error "test1.o")
           (process-wait process)
           (sys-system (cmds "cat" "NoSuchFile" "2>" "test2.o"))
           (let ((r (call-with-input-file "test1.o" port->string))
                 (s (call-with-input-file "test2.o" port->string)))
             (equal? r s)))))

(rmrf "test1.o" "test2.o")

(test* "call-with-output-process" '(#t 1 2)
       (let1 s (call-with-input-file "test.o" port->string)
         (rmrf "test.o")
         (receive (x y)
             (call-with-output-process (cmds "cat" ">" "test.o")
               (lambda (out) (display s out) (values 1 2)))
           (let1 r (call-with-input-file "test.o" port->string)
             (list (equal? r s) x y)))))

(test* "call-with-output-process (redirect)" '(#t 1 2)
       (let1 s (call-with-input-file "test.o" port->string)
         (rmrf "test.o")
         (receive (x y)
             (call-with-output-process (cmd "cat")
               (lambda (out) (display s out) (values 1 2))
               :output "test.o")
           (let1 r (call-with-input-file "test.o" port->string)
             (list (equal? r s) x y)))))

(test* "call-with-output-process (redirect/error - ignore)" #t
       (begin
         (call-with-output-process (cmds "cat" "NoSuchFile")
           (lambda (out) #f)
           :error "test1.o" :on-abnormal-exit :ignore)
         (sys-system (cmds "cat" "NoSuchFile" "2>" "test2.o"))
         (let ((r (call-with-input-file "test1.o" port->string))
               (s (call-with-input-file "test2.o" port->string)))
           (equal? r s))))

(test* "call-with-output-process (redirect/error - raise)" #t
       (guard (e ((<process-abnormal-exit> e)
                  (sys-system (cmds "cat" "NoSuchFile" "2>" "test2.o"))
                  (let ((r (call-with-input-file "test1.o" port->string))
                        (s (call-with-input-file "test2.o" port->string)))
                    (equal? r s))))
         (call-with-output-process (cmds "cat" "NoSuchFile")
           (lambda (out) #f) :error "test1.o")))

;; NB: On Solaris, cat seems to return 2 when the file doesn't exist.
(test* "call-with-output-process (redirect/error - handle)" (test-one-of 1 2)
       (let/cc k
         (call-with-output-process (cmd "cat" 'NoSuchFile)
           port->string
           :error "test1.o"
           :on-abnormal-exit (lambda (p)
                               (k (sys-wait-exit-status
                                   (process-exit-status p)))))))

(rmrf "test1.o" "test2.o")

(test* "with-output-to-process" '(#t 1 2)
       (let1 s (call-with-input-file "test.o" port->string)
         (rmrf "test.o")
         (receive (x y)
             (with-output-to-process (cmds "cat" ">" "test.o")
               (lambda () (display s) (values 1 2)))
           (let1 r (call-with-input-file "test.o" port->string)
             (list (equal? r s) x y)))))

(test* "with-output-to-process (redirect)" '(#t 1 2)
       (let1 s (call-with-input-file "test.o" port->string)
         (rmrf "test.o")
         (receive (x y)
             (with-output-to-process (cmd "cat")
               (lambda () (display s) (values 1 2))
               :output "test.o")
           (let1 r (call-with-input-file "test.o" port->string)
             (list (equal? r s) x y)))))

(test* "call-with-process-io" "test.o\n"
       (let* ((s (call-with-input-file "test.o" port->string))
              (r (call-with-process-io (cmd "grep" "test\\.o")
                   (lambda (i o)
                     (display s o) (close-output-port o)
                     (port->string i)))))
         r))

(test* "call-with-process-io (redirect/error)" #t
       (begin
         (call-with-process-io (cmds "cat" "NoSuchFile")
           (lambda (i o) #f)
           :error "test1.o" :on-abnormal-exit :ignore)
         (sys-system (cmds "cat" "NoSuchFile" "2>" "test2.o"))
         (let ((r (call-with-input-file "test1.o" port->string))
               (s (call-with-input-file "test2.o" port->string)))
           (equal? r s))))

(rmrf "test.o" "test1.o" "test2.o")
(touch "test.o")
(sys-system (cmds "ls" "-a" ">" "test.o"))

(test* "process-output->string" #t
       (let ((r (process-output->string (cmd "ls" '-a)))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r (string-join (string-tokenize s) " "))))

(test* "process-output->string (error - ignore)" ""
       (process-output->string (cmd "cat" "NoSuchFile")
                               :error *nulldev*
                               :on-abnormal-exit :ignore))

(test* "process-output->string (error - raise)"
       (test-error <process-abnormal-exit>)
       (process-output->string (cmd "cat" "NoSuchFile") :error *nulldev*))

(test* "process-output->string-list" #t
       (let ((r (process-output->string-list (cmd "ls" '-a)))
             (s (call-with-input-file "test.o" port->string-list)))
         (equal? r s)))

;; This merely tests if process-output->string-list accepts :encoding
;; argument.  https://github.com/shirok/Gauche/issues/651
(test* "process-output->string-list" #t
       (begin
         (process-output->string-list (cmd "ls" '-a)
                                      :encoding (gauche-character-encoding))
         #t))

(rmrf "test2.o")
(sys-mkdir "test2.o" #o755)
(with-output-to-file "test2.o/probe"
  (^[] (display "Aloha!")))
(test* "process-output->string (different directory)" "Aloha!"
       (process-output->string (cmd-in-subdir "cat" "probe")
                               :directory "test2.o"))

;; process pipeline and port
(let ()
  (rmrf "test.o")
  (with-output-to-file "test.o"
    (^[] (for-each print '("banana" "habana" "tabata" "cabara"))))
  (test* "process-output->string with pipeline"
         '("banana" "habana")
         (process-output->string-list `(,(cmd "cat" "test.o")
                                        ,(cmd "grep" "bana"))))
  )

(rmrf "test2.o")
(rmrf "testc.o")

(cond-expand
 [gauche.os.windows
  ;; Testing fix for vulnerability https://kb.cert.org/vuls/id/123335
  ;; NB: We avoid using call-with-temporary-directory, for file.util
  ;; isn't tested yet when we run this test.
  (define-syntax with-temp-files
    (syntax-rules ()
      [(_ body ...)
       (unwind-protect
           (begin
             (with-output-to-file "test.bat"
               (^[]
                 (print "@ECHO OFF")
                 (print "ECHO %1-%2-%3")))
             (with-output-to-file "test.cmd"
               (^[]
                 (print "@ECHO OFF")
                 (print "ECHO %1-%2-%3")))
             body ...)
         (sys-unlink "test.bat")
         (sys-unlink "test.cmd"))]))

  (with-temp-files
   ;; Taking executable path doesn't work on all platforms, but we know
   ;; it works on Windows.
   (let1 gosh (with-module gauche.internal (%gauche-executable-path))
     (test* "Windows escaping quirks (exe)" "(a b)"
            (process-output->string `(,gosh "-Eprint *argv*" "-Eexit" _ a b)))
     (test* "Windows escaping quirks (exe)" "(\"&whoami)"
            (process-output->string `(,gosh "-Eprint *argv*" "-Eexit" _ "\"&whoami"))))
   (test* "Windows escaping quirks (bat)" "a-b-c"
            (process-output->string `(".\\test.bat" a b c)))
   (test* "Windows escaping quirks (bat)" (test-error <error> #/unsafe/)
            (process-output->string `(".\\test.bat" "\"&whoami")))
   (test* "Windows escaping quirks (cmd)" "a-b-c"
            (process-output->string `(".\\test.cmd" a b c)))
   (test* "Windows escaping quirks (cmd)" (test-error <error> #/unsafe/)
            (process-output->string `(".\\test.cmd" "\"&whoami")))
   )]
 [else])

;;-------------------------------
(test-section "unwind-protect upon exit")

;; unwind-protect is tested in exception.scm, but we needed to do this test
;; after we test gauche.process.

(rmrf "test.o" "test1.o")

(with-output-to-file "test.o"
  (cut write '(define (main args)
                (unwind-protect (begin
                                  (with-output-to-file "test1.o"
                                    (^[] (display "foo\n")))
                                  (exit 1))
                  (sys-unlink "test1.o")))))

(test* "unwind-protect upon exit" '(1 #f)
       ;; assuming we're running gosh under $top_builddir/src
       (let1 p (run-process `("../../src/gosh" "-ftest" "test.o") :wait #t)
         (list (sys-wait-exit-status (process-exit-status p))
               (file-exists? "test1.o"))))

(rmrf "test.o" "test1.o")

(test-end)
