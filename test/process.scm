;;
;; test gauche.process
;;

(use gauche.test)
(use srfi-13)
(test-start "gauche.process")

(use gauche.process)
(test-module 'gauche.process)

;; Check if the programs we'll use are available on the
;; platform.  If not, we don't do further test.
(unless (and (zero? (sys-system "cat /dev/null"))
             (zero? (sys-system "ls > /dev/null"))
             (zero? (sys-system "echo x | grep x > /dev/null")))
  (test-end)
  (exit 0))

;; Avoid locale specific behavior of client programs
(when (global-variable-bound? 'gauche 'sys-putenv)
  (sys-putenv "LANG" "C"))

;;-------------------------------
(test-section "process object")

(sys-system "rm -rf test.o test1.o")
(sys-system "touch test.o")

(test* "run-process" 0
       (let1 p (run-process 'ls :output "test.o")
         (and (process-wait p) (process-exit-status p))))
(test* "run-process" 0
       (let1 p (run-process 'grep "test.o" :input "test.o" :output "/dev/null")
         (and (process-wait p) (process-exit-status p))))
(test* "run-process" 256
       (let1 p (run-process 'grep "NoSuchFile"
                            :input "test.o" :output "/dev/null")
         (and (process-wait p) (process-exit-status p))))

(test* "run-process (output pipe)" '(0 #t)
       (let* ((p  (run-process "cat" "test.o" :output :pipe))
              (in (process-output p))
              (s  (port->string in))
              (c  (call-with-input-file "test.o" port->string))
              (x  (and (process-wait p) (process-exit-status p))))
         (list x (equal? c s))))

(test* "run-process (input pipe)" '(0 #t)
       (let* ((p  (run-process "cat" :input :pipe :output :pipe))
              (out (process-input p))
              (in  (process-output p))
              (s   "test\ntest"))
         (display s out)
         (close-output-port out)
         (let* ((ss (port->string in))
                (x  (and (process-wait p) (process-exit-status p))))
           (list x (equal? s ss)))))

(test* "run-process (error pipe)" #t
       (let* ((p  (run-process "cat" "NoSuchFile" :error :pipe))
              (in (process-error p))
              (s  (port->string in))
              (x  (process-wait p))
              (p1 (run-process "cat" "NoSuchFile" :error "test.o"))
              (s1 (and (process-wait p1)
                       (call-with-input-file "test.o" port->string)))
              )
         (equal? s s1)))

;; NB: how to test :wait and :fork?

(test* "process-kill" SIGKILL
       (let ((p (run-process "cat" :input :pipe :output :pipe
                             :error "/dev/null")))
         (process-kill p)
         (process-wait p)
         (let ((x (process-exit-status p)))
           (and (sys-wait-signaled? x)
                (sys-wait-termsig x)))))

(test* "non-blocking wait" '(#f #t #f)
       (let* ((p  (run-process "cat" :input :pipe :output :pipe
                               :error "/dev/null"))
              (r0 (process-wait p #t))
              (r1 (begin (process-kill p) (process-wait p)))
              (r2 (process-wait p #t))
              )
         (list r0 r1 r2)))

(test* "process-list" '()
       (process-list))

;;-------------------------------
(test-section "process ports")

(sys-system "rm -rf test.o test1.o test2.o")
(sys-system "touch test.o")
(sys-system "ls -a > test.o")

(test* "open-input-process-port" #t
       (receive (p process) (open-input-process-port '(ls -a))
         (let ((r (port->string p))
               (s (call-with-input-file "test.o" port->string)))
           (close-input-port p)
           (process-wait process)
           (equal? r s))))

(test* "open-input-process-port (redirect)" #t
       (receive (p process) (open-input-process-port '(cat) :input "test.o")
         (let ((r (port->string p))
               (s (call-with-input-file "test.o" port->string)))
           (close-input-port p)
           (process-wait process)
           (equal? r s))))

(test* "open-input-process-port (redirect)" #t
       (receive (p process) (open-input-process-port '(cat "NoSuchFile")
                                                     :error "test1.o")
         (process-wait process)
         (sys-system "cat NoSuchFile 2> test2.o")
         (let ((r (call-with-input-file "test1.o" port->string))
               (s (call-with-input-file "test2.o" port->string)))
           (equal? r s))))

(sys-system "rm -f test1.o test2.o")

(test* "call-with-input-process" #t
       (let ((r (call-with-input-process '(ls -a) port->string))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "call-with-input-process" #t
       (let ((r (call-with-input-process "ls -a" port->string))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "call-with-input-process (redirect)" #t
       (let ((r (call-with-input-process '(cat) port->string :input "test.o"))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "call-with-input-process (redirect)" #t
       (begin (call-with-input-process "cat NoSuchFile"
                port->string :error "test1.o")
              (sys-system "cat NoSuchFile 2> test2.o")
              (let ((r (call-with-input-file "test1.o" port->string))
                    (s (call-with-input-file "test2.o" port->string)))
                (equal? r s))))

(sys-system "rm -f test1.o test2.o")

(test* "with-input-from-process" #t
       (let ((r (with-input-from-process '(cat test.o)
                  (lambda () (port->string (current-input-port)))))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "with-input-from-process" #t
       (let ((r (with-input-from-process "cat < test.o"
                  (lambda () (port->string (current-input-port)))))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "with-input-from-process (redirect)" #t
       (let ((r (with-input-from-process '(cat test.o)
                  (lambda () (port->string (current-input-port)))
                  :input "test.o"))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r s)))

(test* "open-output-process-port" #t
       (let1 s (call-with-input-file "test.o" port->string)
         (sys-system "rm -f test.o")
         (receive (p process) (open-output-process-port "cat > test.o")
           (display s p)
           (close-output-port p)
           (process-wait process)
           (let1 r (call-with-input-file "test.o" port->string)
             (equal? r s)))))

(test* "open-output-process-port (redirect)" #t
       (let1 s (call-with-input-file "test.o" port->string)
         (sys-system "rm -f test.o")
         (receive (p process)
             (open-output-process-port '(cat) :output "test.o")
           (display s p)
           (close-output-port p)
           (process-wait process)
           (let1 r (call-with-input-file "test.o" port->string)
             (equal? r s)))))

(test* "open-output-process-port (redirect)" #t
       (let1 s (call-with-input-file "test.o" port->string)
         (receive (p process)
             (open-output-process-port "cat NoSuchFile" :error "test1.o")
           (process-wait process)
           (sys-system "cat NoSuchFile 2> test2.o")
           (let ((r (call-with-input-file "test1.o" port->string))
                 (s (call-with-input-file "test2.o" port->string)))
             (equal? r s)))))

(sys-system "rm -f test1.o test2.o")

(test* "call-with-output-process" '(#t 1 2)
       (let1 s (call-with-input-file "test.o" port->string)
         (sys-system "rm -f test.o")
         (receive (x y)
             (call-with-output-process "cat > test.o"
               (lambda (out) (display s out) (values 1 2)))
           (let1 r (call-with-input-file "test.o" port->string)
             (list (equal? r s) x y)))))

(test* "call-with-output-process (redirect)" '(#t 1 2)
       (let1 s (call-with-input-file "test.o" port->string)
         (sys-system "rm -f test.o")
         (receive (x y)
             (call-with-output-process '(cat)
               (lambda (out) (display s out) (values 1 2))
               :output "test.o")
           (let1 r (call-with-input-file "test.o" port->string)
             (list (equal? r s) x y)))))
       
(test* "call-with-output-process (redirect)" #t
       (let1 s (call-with-input-file "test.o" port->string)
         (call-with-output-process "cat NoSuchFile"
           (lambda (out) #f) :error "test1.o")
         (sys-system "cat NoSuchFile 2> test2.o")
         (let ((r (call-with-input-file "test1.o" port->string))
               (s (call-with-input-file "test2.o" port->string)))
           (equal? r s))))

(sys-system "rm -f test1.o test2.o")

(test* "with-output-to-process" '(#t 1 2)
       (let1 s (call-with-input-file "test.o" port->string)
         (sys-system "rm -f test.o")
         (receive (x y)
             (with-output-to-process "cat > test.o"
               (lambda () (display s) (values 1 2)))
           (let1 r (call-with-input-file "test.o" port->string)
             (list (equal? r s) x y)))))

(test* "with-output-to-process (redirect)" '(#t 1 2)
       (let1 s (call-with-input-file "test.o" port->string)
         (sys-system "rm -f test.o")
         (receive (x y)
             (with-output-to-process '(cat)
               (lambda () (display s) (values 1 2))
               :output "test.o")
           (let1 r (call-with-input-file "test.o" port->string)
             (list (equal? r s) x y)))))

(test* "call-with-process-io" "test.o\n"
       (let* ((s (call-with-input-file "test.o" port->string))
              (r (call-with-process-io '(grep "test\\.o")
                   (lambda (i o)
                     (display s o) (close-output-port o)
                     (port->string i)))))
         r))

(test* "call-with-process-io (redirect)" #t
       (begin
         (call-with-process-io "cat NoSuchFile"
           (lambda (i o) #f) :error "test1.o")
         (sys-system "cat NoSuchFile 2> test2.o")
         (let ((r (call-with-input-file "test1.o" port->string))
               (s (call-with-input-file "test2.o" port->string)))
           (equal? r s))))

(sys-system "rm -rf test.o test1.o test2.o")
(sys-system "touch test.o")
(sys-system "ls -a > test.o")

(test* "process-output->string" #t
       (let ((r (process-output->string '(ls -a)))
             (s (call-with-input-file "test.o" port->string)))
         (equal? r (string-join (string-tokenize s) " "))))

(test* "process-output->string-list" #t
       (let ((r (process-output->string-list '(ls -a)))
             (s (call-with-input-file "test.o" port->string-list)))
         (equal? r s)))

(test-end)
