;;
;; test for system related procedures
;;

(use gauche.test)
(use srfi-1)
(use srfi-11)                           ;let-values
(use srfi-13)

(test-start "system")

;;-------------------------------------------------------------------
(test-section "system")
;; test this first, so that we can use system commands to verify our results.

(test* "system" #t (begin (sys-system ":") #t))
(test* "system" #t (begin (sys-system "") #t))

(define (get-command-output command)
  (sys-system "rm -rf test.out")
  (sys-system (format #f "~a > test.out" command))
  (call-with-input-file "test.out"
    (lambda (in)
      (let loop ((line (read-line in)) (lines '()))
        (if (eof-object? line)
            (begin (sys-system "rm -rf test.out")
                   (string-join (reverse lines) " "))
            (loop (read-line in) (cons line lines)))))))

(define (get-lsmode file)
  (string-take (get-command-output (format #f "ls -ld ~a" file)) 10))

(define (get-pwd-via-pwd)
  ;; use pwd command to get pwd.  avoid using shell's built-in pwd,
  ;; for it may be confused by symlinks.
  (cond ((sys-access "/bin/pwd" |X_OK|) (get-command-output "/bin/pwd"))
        ((sys-access "/usr/bin/pwd" |X_OK|) (get-command-output "/usr/bin/pwd"))
        ((sys-access "/sbin/pwd" |X_OK|) (get-command-output "/sbin/pwd"))
        (else (get-command-output "pwd"))))

;;-------------------------------------------------------------------
(test-section "environment")

(test* "getenv" (get-command-output "echo $PATH")
       (sys-getenv "PATH"))

(test* "getenv"
       (let ((x (get-command-output "echo $NoSucHEniIRoNmenT")))
         (if (string-null? x) #f x))
       (sys-getenv "NoSucHEniIRoNmenT"))

(test* "getcwd" (get-pwd-via-pwd)
       (sys-getcwd))

;;-------------------------------------------------------------------
(test-section "pathnames")

(test* "basename" "ghi.jkl" (sys-basename "/abc/def/ghi.jkl"))
(test* "dirname" "/abc/def" (sys-dirname "/abc/def/ghi.jkl"))
(test* "basename" "ghi.jkl" (sys-basename "/abc/def/ghi.jkl/"))
(test* "dirname" "/abc/def" (sys-dirname "/abc/def/ghi.jkl/"))
(test* "basename" "ghi.jkl" (sys-basename "/abc//def//ghi.jkl//"))
(test* "dirname" "/abc//def" (sys-dirname "/abc//def//ghi.jkl//"))
(test* "basename" "ghi.jkl" (sys-basename "ghi.jkl"))
(test* "dirname" "." (sys-dirname "ghi.jkl"))

(test* "basename" "" (sys-basename ""))
(test* "dirname"  "." (sys-dirname ""))
(test* "basename" "" (sys-basename "/"))
(test* "dirname"  "/" (sys-dirname "/"))
(test* "basename" "" (sys-basename "//"))
(test* "dirname"  "/" (sys-dirname "//"))

(test* "basename" ".." (sys-basename "../"))
(test* "dirname"  "." (sys-dirname "../"))
(test* "basename" ".." (sys-basename "../.."))
(test* "dirname"  ".." (sys-dirname "../.."))

(test* "normalize" (string-append (get-pwd-via-pwd) "/.")
       (sys-normalize-pathname "." :absolute #t))
(test* "normalize" (string-append (get-pwd-via-pwd) "/")
       (sys-normalize-pathname "" :absolute #t))
(test* "normalize" (string-append (get-command-output "echo $HOME") "/abc")
       (sys-normalize-pathname "~/abc" :expand #t))
(test* "normalize" "/a/b/c/d/e"
       (sys-normalize-pathname "/a/b//.///c//d/./e"
                               :canonicalize #t))
(test* "normalize" "/a/b/c/d/e/"
       (sys-normalize-pathname "/a/b//.///c//d/./e/"
                               :canonicalize #t))
(test* "normalize" "/a/b/c/d/e/"
       (sys-normalize-pathname "/a/B//./../c/d/../../b//c/d/e/f/.."
                               :canonicalize #t))
(test* "normalize" ""
       (sys-normalize-pathname ""
                               :canonicalize #t))
(test* "normalize" "a/../../.."
       (sys-normalize-pathname "a/b/c/../../../../.."
                               :canonicalize #t))

;;-------------------------------------------------------------------
(test-section "filesystem")

(sys-system "rm -rf test.dir >/dev/null")

(test* "access" '(#f #f #f #f)
       (map (lambda (flag) (sys-access "test.dir" flag))
            (list |F_OK| |R_OK| |W_OK| |X_OK|)))
(sys-system "touch test.dir")
(sys-system "chmod 777 test.dir")
(test* "access" '(#t #t #t #t)
       (map (lambda (flag) (sys-access "test.dir" flag))
            (list |F_OK| |R_OK| |W_OK| |X_OK|)))
(sys-system "chmod 500 test.dir")
(test* "access" '(#t #t #f #t)
       (map (lambda (flag) (sys-access "test.dir" flag))
            (list |F_OK| |R_OK| |W_OK| |X_OK|)))
(sys-system "chmod 477 test.dir")
(test* "access" '(#t #t #f #f)
       (map (lambda (flag) (sys-access "test.dir" flag))
            (list |F_OK| |R_OK| |W_OK| |X_OK|)))
(sys-system "chmod 000 test.dir")
(test* "access" '(#t #f #f #f)
       (map (lambda (flag) (sys-access "test.dir" flag))
            (list |F_OK| |R_OK| |W_OK| |X_OK|)))

(test* "unlink" #f
       (begin
         (sys-unlink "test.dir") (sys-access "test.dir" |F_OK|)))

(test* "mkdir" "drwxr-x---"
       (begin
         (sys-mkdir "test.dir" #o750)
         (get-lsmode "test.dir")))

(test* "chmod" "drwxr-xr-x"
       (begin
         (sys-chmod "test.dir" #o755)
         (get-lsmode "test.dir")))

(with-output-to-file "test.dir/xyzzy"
  (lambda () (display "zzzzzZzzzzZZzZzzzzzzzZzzZZZZz") (newline)))

(test* "rename" '(#f #t)
       (begin
         (sys-rename "test.dir/xyzzy" "test.dir/zzZzz")
         (list (sys-access "test.dir/xyzzy" |F_OK|)
               (sys-access "test.dir/zzZzz" |F_OK|))))

(test* "readdir" '("." ".." "zzZzz")
       (sort (sys-readdir "test.dir")))

(test* "link" '("." ".." "xyzzy" "zzZzz")
       (begin
         (sys-link "test.dir/zzZzz" "test.dir/xyzzy")
         (sort (sys-readdir "test.dir"))))

(test* "unlink" '("." ".." "xyzzy")
       (begin
         (sys-unlink "test.dir/zzZzz")
         (sort (sys-readdir "test.dir"))))

(test* "rename" '("." ".." "zzZzz")
       (begin
         (sys-rename "test.dir/xyzzy" "test.dir/zzZzz")
         (sort (sys-readdir "test.dir"))))

(test* "rmdir" #f
       (begin
         (sys-unlink "test.dir/zzZzz")
         (sys-rmdir "test.dir")
         (sys-access "test.dir" |F_OK|)))

;;-------------------------------------------------------------------
(test-section "stat")

(sys-system "rm -rf test.dir > /dev/null")
(with-output-to-file "test.dir" (lambda () (display "01234")))
(sys-chmod "test.dir" #o654)

(test* "stat" '(#o654 regular 5)
       (let ((s (sys-stat "test.dir")))
         (list (logand #o777 (sys-stat->mode s))
               (sys-stat->file-type s)
               (sys-stat->size s))))

(test* "fstat" '(#o654 regular 5)
       (call-with-input-file "test.dir"
         (lambda (p)
           (let ((s (sys-fstat p)))
             (list (logand #o777 (sys-stat->mode s))
                   (sys-stat->file-type s)
                   (sys-stat->size s))))))

(sys-unlink "test.dir")
(sys-mkdir "test.dir" #o700)

(test* "stat" '(#o700 directory)
       (let ((s (sys-stat "test.dir")))
         (list (logand #o777 (sys-stat->mode s))
               (sys-stat->file-type s))))

(test* "fstat" '(#o700 directory)
       (call-with-input-file "test.dir"
         (lambda (p)
           (let ((s (sys-fstat p)))
             (list (logand #o777 (sys-stat->mode s))
                   (sys-stat->file-type s))))))

(sys-rmdir "test.dir")

;;-------------------------------------------------------------------
(test-section "pipe")

(test* "pipe" "abc"
       (receive (in out) (sys-pipe)
         (display "abc\n" out) (flush out)
         (let1 r (read-line in)
           (close-input-port in)
           (close-output-port out)
           r)))

(test* "pipe and char-ready? (none)" '(#f #t #f)
       (receive (in out) (sys-pipe :buffering :none)
         (display "a" out) (read-char in)
         (let1 f1 (char-ready? in)
           (display "bc" out) (read-char in)
           (let1 f2 (char-ready? in)
             (read-char in)
             (let1 f3 (char-ready? in)
               (close-input-port in) (close-output-port out)
               (list f1 f2 f3))))))

(test* "pipe and char-ready? (line)" '(#f #t #t)
       (receive (in out) (sys-pipe :buffering :line)
         (display "a" out)
         (let1 f1 (char-ready? in)
           (display "\n" out)
           (let1 f2 (char-ready? in)
             (read-char in)
             (let1 f3 (char-ready? in)
               (close-input-port in) (close-output-port out)
               (list f1 f2 f3))))))

(test* "pipe and char-ready? (full)" '(#f #f #t)
       (receive (in out) (sys-pipe :buffering :full)
         (display "a" out)
         (let1 f1 (char-ready? in)
           (display "\n" out)
           (let1 f2 (char-ready? in)
             (flush out) (read-char in)
             (let1 f3 (char-ready? in)
               (close-input-port in) (close-output-port out)
               (list f1 f2 f3))))))

(test* "pipe and read-block(none)" 2
       (receive (in out) (sys-pipe :buffering :none)
         (display "ab" out)
         (let1 r (string-size (read-block 1000 in))
           (close-input-port in) (close-output-port out)
           r)))

(test* "pipe and read-block(line)" 2
       (receive (in out) (sys-pipe :buffering :line)
         (display "a\n" out)
         (let1 r (string-size (read-block 1000 in))
           (close-input-port in) (close-output-port out)
           r)))

;;-------------------------------------------------------------------
(test-section "fork&exec")

(test* "fork & wait" #t
       (let ((pid (sys-fork)))
         (if (= pid 0)
             (sys-exit 5)
             (receive (rpid code) (sys-wait)
               (and (= rpid pid)
                    (sys-wait-exited? code)
                    (= (sys-wait-exit-status code) 5))))))

(test* "fork & waitpid" #t
       (let ((pid (sys-fork)))
         (if (= pid 0)
             (sys-exit 10)
             (receive (rpid code) (sys-waitpid pid)
               (and (= rpid pid)
                    (sys-wait-exited? code)
                    (= (sys-wait-exit-status code) 10))))))

(test* "fork, wait & kill" #t
       (let ((pid (sys-fork)))
         (if (= pid 0)
             (begin (sys-pause) (sys-exit 0))
             (begin 
               (sys-kill pid |SIGKILL|)
               (receive (rpid code) (sys-wait)
                 (and (= rpid pid)
                      (sys-wait-signaled? code)
                      (= (sys-wait-termsig code) |SIGKILL|)))))))

(test* "fork, wait, kill & sleep" #t
       (let1 pid (sys-fork)
         (if (= pid 0)
             (begin (sys-sleep 1) (sys-exit 0))
             (begin 
               (sys-kill pid |SIGSTOP|) 
               (receive (rpid code) (sys-waitpid pid :untraced #t)
                 (and (= rpid pid)
                      (sys-wait-stopped? code)
                      (= (sys-wait-stopsig code) |SIGSTOP|)
                      (begin (sys-kill pid |SIGCONT|)
                             (receive (rpid code) (sys-wait)
                               (and (= rpid pid)
                                    (sys-wait-exited? code)
                                    (= (sys-wait-exit-status code) 0)
                                    )))
                      )))
             ))
       )

(test* "fork & pipe" 70000
       (receive (in out) (sys-pipe)
         (let1 pid (sys-fork)
           (if (= pid 0)
               (begin (close-input-port in)
                      (display (make-string 69999) out)
                      (with-error-handler
                          (lambda (e) (sys-exit 0))
                        (lambda ()
                          (newline out)
                          (close-output-port out)
                          (sys-pause))))
               (let loop ((toread 70000)
                          (nread  0))
                 (let1 r (string-size (read-block toread in))
                   (if (>= (+ nread r) 70000)
                       (begin (sys-kill pid SIGTERM)
                              (sys-waitpid pid)
                              (+ nread r))
                       (loop (- toread r) (+ nread r)))))
               ))))

;;-------------------------------------------------------------------
(test-section "select")

(test* "fdset" '(3 #t #f #t #t #f)
       (let ((fdset (make <sys-fdset>)))
         (set! (sys-fdset-ref fdset (current-input-port)) #t)
         (sys-fdset-set! fdset (current-error-port) #t)
         (sys-fdset-set! fdset 3 #t)
         (sys-fdset-set! fdset 4 #f)
         (cons (sys-fdset-max-fd fdset)
               (map (lambda (i) (sys-fdset-ref fdset i)) (iota 5)))))

(test* "fdset" '(-1 7 7 4 10 10 -1)
       (let ((fdset (make <sys-fdset>))
             (result '()))
         (define (push-result)
           (set! result (cons (sys-fdset-max-fd fdset) result)))
         (push-result)
         (sys-fdset-set! fdset 7 #t)
         (push-result)
         (sys-fdset-set! fdset 4 #t)
         (push-result)
         (sys-fdset-set! fdset 7 #f)
         (push-result)
         (sys-fdset-set! fdset 10 #t)
         (push-result)
         (sys-fdset-set! fdset 4 #f)
         (push-result)
         (sys-fdset-set! fdset 10 #f)
         (push-result)
         (reverse result)))

(test* "select" '(0 #f #f #f #f 1 #t #f #f #t #\x)
       (let*-values (((in out) (sys-pipe))
                     ((pid) (sys-fork)))
         (if (= pid 0)
             (begin (sys-select #f #f #f 100000)
                    (display "x" out)
                    (close-output-port out)
                    (sys-exit 0))
             (let ((rfds (make <sys-fdset>)))
               (sys-fdset-set! rfds in #t)
               (receive (an ar aw ae)
                   (sys-select rfds #f #f 0)
                 (receive (bn br bw be)
                     (sys-select! rfds #f #f #f)
                   (begin0
                    (list an (eq? ar rfds) aw ae
                          (sys-fdset-ref ar in)
                          bn (eq? br rfds) bw be
                          (sys-fdset-ref rfds in)
                          (read-char in))
                    (sys-waitpid pid)))))
             ))
       )

;;-------------------------------------------------------------------
(test-section "signal handling")

(test* "sigalrm1" SIGALRM
       (call/cc
        (lambda (k)
          (with-signal-handlers
           ((SIGALRM => k)
            (#t (k 0)))
           (lambda ()
             (sys-alarm 1)
             (sys-pause))))))

(test* "sigalrm2" 0
       (call/cc
        (lambda (k)
          (with-signal-handlers
           ((#t (k 0))
            (SIGALRM => k))
           (lambda ()
             (sys-alarm 1)
             (sys-pause))))))

(test* "sigalrm3" *test-error*
       (call/cc
        (lambda (k)
          (with-signal-handlers
           ((SIGINT => k)
            (SIGUSR1 => k))
           (lambda ()
             (sys-alarm 1)
             (sys-pause))))))

(test* "fork & sigint" #t
       (let ((pid (sys-fork))
             (sigint  #f)
             (sigchld #f))
         (if (= pid 0)
             (let ((parent (sys-getppid)))
               (sys-sleep 1)
               (sys-kill parent SIGINT)
               (sys-exit 0))
             (with-signal-handlers
              ((SIGINT  (set! sigint #t))
               (SIGCHLD (sys-waitpid pid) (set! sigchld #t)))
              (lambda ()
                (let loop ()
                  (if (and sigint sigchld)
                      #t
                      (begin (sys-pause) (loop))))))
             )))

(test* "sigchld" SIGCHLD
       (call/cc
        (lambda (k)
          (with-signal-handlers
           ((SIGCHLD (sys-wait) (k SIGCHLD)))
           (lambda ()
             (let ((pid (sys-fork)))
               (if (= pid 0)
                   (sys-exit 0)
                   (sys-pause))))))))

(test* "sigmask" 'hup
       (let ((sig #f)
             (chld #f)
             (mask1 (sys-sigset-add! (make <sys-sigset>) SIGINT)))
         (call/cc
          (lambda (k)
            (set-signal-handler! SIGINT  k)
            (set-signal-handler! SIGCHLD (lambda (k) (sys-wait) (set! chld #t)))
            (set-signal-handler! SIGHUP  (lambda (k) (set! sig 'hup)))
            (sys-sigmask SIG_BLOCK mask1)
            (let ((pid (sys-fork)))
              (if (= pid 0)
                  (begin
                    (sys-kill (sys-getppid) SIGINT)
                    (sys-kill (sys-getppid) SIGHUP)
                    (sys-exit 0))
                  (begin
                    (let loop ()
                      (unless sig (loop)))
                    (set-signal-handler! SIGINT #f)
                    (sys-sigmask SIG_UNBLOCK mask1)
                    ;;Some systems appear to lose this SIGCHLD (esp. cygwin)
                    ;;(let loop ()
                    ;;  (unless chld (loop)))
                    sig)))))))


(test-end)

