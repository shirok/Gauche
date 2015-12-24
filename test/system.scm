;;
;; test for system related procedures
;;

#!no-fold-case

(use gauche.test)
(use gauche.config)
(use srfi-1)
(use srfi-11)                           ;let-values
(use srfi-13)

(test-start "system")

;;-------------------------------------------------------------------
(test-section "system")
;; test this first, so that we can use system commands to verify our results.

(test* "system" #t (begin (sys-system ":") #t))
(test* "system" #t (begin (sys-system "") #t))

;; shorthand of normalizing pathname.  this doesn't do anything on
;; unix, but on Windows the separator in PATHNAME is replaced.
(define (n pathname) (sys-normalize-pathname pathname))

;; some common operations via command
(define (cmd-rmrf dir)
  (cond-expand
   (gauche.os.windows
    (sys-system #"rmdir /q /s ~(n dir) > NUL 2>&1")
    (sys-system #"del /q ~(n dir) > NUL 2>&1"))
   (else
    (sys-system #"rm -rf ~dir > /dev/null"))))

(define (cmd-mkdir dir)
  (cond-expand
   (gauche.os.windows (sys-system #"mkdir ~(n dir)"))
   (else (sys-system #"mkdir ~dir"))))

(define (cmd-touch path)
  (cond-expand
   (gauche.os.windows (sys-system #"echo \"\" > ~(n path)"))
   (else (sys-system #"touch ~path"))))

(define (get-command-output command)
  (cmd-rmrf "test.out")
  (sys-system (format #f "~a > test.out" command))
  (call-with-input-file "test.out"
    (lambda (in)
      (let loop ((line (read-line in)) (lines '()))
        (if (eof-object? line)
            (begin (close-input-port in)
                   (cmd-rmrf "test.out")
                   (string-join (reverse lines) " "))
            (loop (read-line in) (cons line lines)))))))

(define (get-lsmode file)
  (string-take (get-command-output (format #f "ls -ld ~a" file)) 10))

(define (get-pwd-via-pwd)
  ;; use pwd command to get pwd.  avoid using shell's built-in pwd,
  ;; for it may be confused by symlinks.
  (cond-expand
   (gauche.os.windows (get-command-output "cd"))
   (else
    (cond
     [(sys-access "/bin/pwd" X_OK)
      ;; On MacOSX, /bin/pwd returns _logical_ pathname by default,
      ;; which is IMHO a bad decision (the behavior is /bin/pwd -L in
      ;; other BSDs).  Anyway we have to cope with it.
      (if (string-contains (gauche-config "--arch") "darwin")
        (get-command-output "/bin/pwd -P")
        (get-command-output "/bin/pwd"))]
     [(sys-access "/usr/bin/pwd" X_OK) (get-command-output "/usr/bin/pwd")]
     [(sys-access "/sbin/pwd" X_OK) (get-command-output "/sbin/pwd")]
     [else (get-command-output "pwd")]))))

;;-------------------------------------------------------------------
(test-section "environment")

(test* "getenv"
       (string-trim-both
        (get-command-output (cond-expand
                             (gauche.os.windows "echo %PATH%")
                             (else "echo $PATH"))))
       (sys-getenv "PATH"))

(test* "getcwd" (get-pwd-via-pwd)
       (sys-getcwd))

;; putenv

(cond-expand
 [gauche.sys.setenv
  (test* "sys-putenv" "foo"
         (begin
           (sys-putenv "ZZGGGBBB=foo")
           (sys-getenv "ZZGGGBBB")))
  (test* "sys-putenv" "foo"
         (begin
           (sys-putenv "ZZGGGBBB" "foo")  ;;old API
           (sys-getenv "ZZGGGBBB")))]
 [else])

;; setenv

(cond-expand
 [gauche.sys.setenv
  (test* "sys-setenv" "foo"
         (begin
           (sys-setenv "ZZGGGBBB" "foo" #t)
           (sys-getenv "ZZGGGBBB")))
  (test* "sys-setenv" "foo"
         (begin
           (sys-setenv "ZZGGGBBB" "bar" #f)
           (sys-getenv "ZZGGGBBB")))]
 [else])

;; unsetenv

(cond-expand
 [gauche.sys.unsetenv
  (test* "sys-unsetenv" #f
         (begin
           (sys-setenv "ZZGGGBBB" "foo" #t)
           (sys-unsetenv "ZZGGGBBB")
           (sys-getenv "ZZGGGBBB")))]
 [else])

;; environ
(test* "sys-environ->alist" '(("A" . "B") ("A" . "") ("" . "B") ("A" . "B=C"))
       (sys-environ->alist '("A=B" "A=" "=B" "A=B=C")))

(let ([envs (sys-environ)])
  (define (env-test var)
    (test* #"sys-environ (~var)" #t
           (cond [(sys-getenv var)
                  => (^[val] (boolean (member #"~|var|=~|val|" envs)))]
                 [else #t])))
  (env-test "HOME")
  (env-test "USER")
  (env-test "LANG")
  (env-test "PWD")
  (env-test "TERM")
  (env-test "SHELL"))

;;-------------------------------------------------------------------
(test-section "pathnames")

(test* "basename" "ghi.jkl" (sys-basename "/abc/def/ghi.jkl"))
(test* "dirname"  "/abc/def" (sys-dirname "/abc/def/ghi.jkl"))
(test* "basename" "ghi.jkl" (sys-basename "/abc/def/ghi.jkl/"))
(test* "dirname"  "/abc/def" (sys-dirname "/abc/def/ghi.jkl/"))
(test* "basename" "ghi.jkl" (sys-basename "/abc//def//ghi.jkl//"))
(test* "dirname"  "/abc//def" (sys-dirname "/abc//def//ghi.jkl//"))
(test* "basename" "ghi.jkl" (sys-basename "ghi.jkl"))
(test* "dirname" "." (sys-dirname "ghi.jkl"))

(test* "basename" "" (sys-basename ""))
(test* "dirname"  "." (sys-dirname ""))
(test* "basename" "" (sys-basename "/"))
(test* "dirname"  (n "/") (sys-dirname "/"))
(test* "basename" "" (sys-basename "//"))
(test* "dirname"  (n "/") (sys-dirname "//"))
(test* "basename" "abc"   (sys-basename "/abc"))
(test* "dirname"  (n "/") (sys-dirname  "/abc"))
(test* "basename" "abc"   (sys-basename "//abc"))
(test* "dirname"  (n "/") (sys-dirname  "//abc"))

(test* "basename" ".." (sys-basename "../"))
(test* "dirname"  "." (sys-dirname "../"))
(test* "basename" ".." (sys-basename "../.."))
(test* "dirname"  ".." (sys-dirname "../.."))

(cond-expand
 (gauche.os.windows
  ;; test with a drive letter
  (test* "dirname"  "d:\\" (sys-dirname  "d:"))
  (test* "basename" ""     (sys-basename "d:"))
  (test* "dirname"  "d:\\" (sys-dirname  "d:/"))
  (test* "basename" ""     (sys-basename "d:/"))
  (test* "dirname"  "d:\\" (sys-dirname  "d:/z"))
  (test* "basename" "z"    (sys-basename "d:/z"))
  (test* "dirname"  "d:/z" (sys-dirname  "d:/z/y"))
  (test* "basename" "y"    (sys-basename "d:/z/y"))
  (test* "dirname"  "d:."  (sys-dirname  "d:z"))
  (test* "basename" "z"    (sys-basename "d:z"))
  (test* "dirname"  "d:z"  (sys-dirname  "d:z/y"))
  (test* "basename" "y"    (sys-basename "d:z/y"))
  )
 (else #f))

(test* "normalize" (n (string-append (get-pwd-via-pwd) "/."))
       (sys-normalize-pathname "." :absolute #t))
(test* "normalize" (n (string-append (get-pwd-via-pwd) "/"))
       (sys-normalize-pathname "" :absolute #t))
(cond-expand
 (gauche.os.windows #t)
 (else
  (test* "normalize"
         (n (string-append (slot-ref (sys-getpwuid (sys-getuid)) 'dir) "/abc"))
         (sys-normalize-pathname "~/abc" :expand #t))))

(test* "normalize" (n "/a/b/c/d/e")
       (sys-normalize-pathname "/a/b//.///c//d/./e"
                               :canonicalize #t))
(test* "normalize" (n "/a/b/c/d/e/")
       (sys-normalize-pathname "/a/b//.///c//d/./e/"
                               :canonicalize #t))
(test* "normalize" (n "/a/b/c/d/e/")
       (sys-normalize-pathname "/a/B//./../c/d/../../b//c/d/e/f/.."
                               :canonicalize #t))
(test* "normalize" (n "/a/b/")
       (sys-normalize-pathname "/a/b/c/d/../.."
                               :canonicalize #t))
(test* "normalize" (n "/c/d/")
       (sys-normalize-pathname "/c/d/e/f/../../"
                               :canonicalize #t))
(test* "normalize" (n "/e/f/")
       (sys-normalize-pathname "/e/f/g/h/../../."
                               :canonicalize #t))

(test* "normalize" ""
       (sys-normalize-pathname ""
                               :canonicalize #t))
(test* "normalize" (n "../..")
       (sys-normalize-pathname "a/b/c/../../../../.."
                               :canonicalize #t))
(test* "normalize" (n "../../x/y")
       (sys-normalize-pathname "a/b/c/../../../../../x/y"
                               :canonicalize #t))

;;-------------------------------------------------------------------
(test-section "filesystem")

(cmd-rmrf "test.dir")

(test* "access" '(#f #f #f #f)
       (map (lambda (flag) (sys-access "test.dir" flag))
            (list F_OK R_OK W_OK X_OK)))

(cmd-touch "test.dir")

(test* "unlink" #f
       (begin
         (sys-unlink "test.dir") (sys-access "test.dir" F_OK)))

(cond-expand
 (gauche.os.windows
  ;; we need entirey different scheme here, but for the time being we
  ;; just omit the test.
  (sys-mkdir "test.dir" #o750)
  )
 (else
  (test* "mkdir" #/drw[sx]r-[sx]---/
         (begin
           (sys-mkdir "test.dir" #o750)
           (get-lsmode "test.dir"))
         rxmatch)

  (test* "chmod" #/drw[sx]r-[sx]r-x/
         (begin
           (sys-chmod "test.dir" #o755)
           (get-lsmode "test.dir"))
         rxmatch)

  (test* "fchmod" #/drw[sx]r-[sx]---/
         (begin
           (call-with-input-file "test.dir"
             (cut sys-fchmod <> #o750))
           (get-lsmode "test.dir"))
         rxmatch)
  ))

(define *fs-test-str* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(with-output-to-file "test.dir/xyzzy"
  (lambda () (display *fs-test-str*)))

(test* "rename" '(#f #t)
       (begin
         (sys-rename "test.dir/xyzzy" "test.dir/zzZzz")
         (list (sys-access "test.dir/xyzzy" F_OK)
               (sys-access "test.dir/zzZzz" F_OK))))

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

(test* "truncate" "abcdefghijklmno"
       (begin
         (sys-truncate "test.dir/zzZzz" 15)
         (call-with-input-file "test.dir/zzZzz" read-line)))

(test* "ftruncate" "abcde"
       (begin
         (call-with-output-file "test.dir/zzZzz"
           (cut sys-ftruncate <> 5)
           :if-exists :append)
         (call-with-input-file "test.dir/zzZzz" read-line)))

(test* "rmdir" #f
       (begin
         (sys-unlink "test.dir/zzZzz")
         (sys-rmdir "test.dir")
         (sys-access "test.dir" F_OK)))

;; sys-realpath

(define (expected-path p)
  (let1 pp (sys-normalize-pathname p :absolute #t :canonicalize #t)
    (if (eqv? (string-ref pp (- (string-length pp) 1)) #\/)
      (substring pp 0 (- (string-length pp) 1))
      pp)))

(test* "sys-realpath (/)" (sys-normalize-pathname "/") (sys-realpath "/"))
(test* "sys-realpath (.)" (expected-path ".") (sys-realpath "."))

(cond-expand
 [gauche.sys.symlink
  (sys-unlink "test1.o")
  (sys-unlink "test2.o")
  (with-output-to-file "test1.o" (cut print))

  (sys-symlink "test1.o" "test2.o")
  (test* "sys-realpath (symlink)"
         (expected-path "./test1.o")
         (sys-realpath "./test2.o"))
  (sys-unlink "test2.o")

  (sys-symlink "./test1.o" "test2.o")
  (test* "sys-realpath (symlink)"
         (expected-path "./test1.o")
         (sys-realpath "./test2.o"))
  (sys-unlink "test2.o")

  (sys-symlink "../src/test1.o" "test2.o")
  (test* "sys-realpath (symlink)"
         (expected-path "./test1.o")
         (sys-realpath "./test2.o"))

  (sys-unlink "test1.o")
  (test* "sys-realpath (dangling)"
         (test-error)
         (sys-realpath "./test2.o"))

  (sys-mkdir "test1.o" #o777)
  (with-output-to-file "test1.o/test.o" (cut print))
  (test* "sys-realpath (symlink to dir)"
         (expected-path "./test1.o/test.o")
         (sys-realpath "./test2.o/test.o"))

  (sys-unlink "test1.o/test.o")
  (sys-rmdir "test1.o")
  (sys-unlink "test2.o")

  (test* "sys-realpath (NOENT)"
         (test-error <system-error>)
         (sys-realpath "./test2.o/test.o"))
  ]
 [else])

;; sys-mkstemp/sys-mkdtemp
(let ()
  ;; Check if multiple temporary files or directories are created.
  (define (test-mkxtemp name creator tester remover)
    (test* name '(#t #t)
       (let1 temps (map creator (iota 3))
         (begin0 (list (every tester temps)
                       (let1 sorted (sort temps)
                         (every (complement equal?) sorted (cdr sorted))))
           (for-each remover temps)))))

  (test-mkxtemp "sys-mkstemp"
                (^_ (receive (p n) (sys-mkstemp "test.o")
                      (close-output-port p)
                      n))
                file-is-regular?
                sys-unlink)

  (test-mkxtemp "sys-mkdtemp"
                (^_ (sys-mkdtemp "test.dir"))
                file-is-directory?
                sys-rmdir))

;;-------------------------------------------------------------------
(test-section "time")

(test* "srfi time" #t (time? (current-time)))
(test* "srfi time time->seconds" #t
       (let1 t (current-time)
         (= (ref t'second) (floor->exact (time->seconds t)))))
(test* "srfi time seconds->time" '(98765432109876 500000000)
       (let1 t (seconds->time 98765432109876.5)
         (list (ref t'second) (ref t'nanosecond))))
(test* "srfi time setter" (make <time> :second -98765432109876 :nanosecond 4)
       (let1 t (make <time>)
         (set! (ref t'second) -98765432109876)
         (set! (ref t'nanosecond) 4)
         t))

;;-------------------------------------------------------------------
(test-section "stat")

(let ()
  (define (mask unix win)
    (cond-expand
     (gauche.os.windows win)
     (else unix)))

  (cmd-rmrf "test.dir")
  (with-output-to-file "test.dir" (lambda () (display "01234")))
  (sys-chmod "test.dir" #o654)


  (test* "stat" `(,(mask #o654 #o666) regular 5)
         (let ((s (sys-stat "test.dir")))
           (list (logand #o777 (sys-stat->mode s))
                 (sys-stat->file-type s)
                 (sys-stat->size s))))

  (test* "fstat" `(,(mask #o654 #o666) regular 5)
         (call-with-input-file "test.dir"
           (lambda (p)
             (let ((s (sys-fstat p)))
               (list (logand #o777 (sys-stat->mode s))
                     (sys-stat->file-type s)
                     (sys-stat->size s))))))

  (sys-unlink "test.dir")
  (sys-mkdir "test.dir" #o700)

  (test* "stat" `(,(mask #o700 #o777) directory)
         (let ((s (sys-stat "test.dir")))
           (list (logand #o777 (sys-stat->mode s))
                 (sys-stat->file-type s))))

  ;; on windows you cannot use open-input-file on a directory.
  (cond-expand
   (gauche.os.windows)
   (else
    (test* "fstat" `(,(mask #o700 #o777) directory)
           (call-with-input-file "test.dir"
             (lambda (p)
               (let ((s (sys-fstat p)))
                 (list (logand #o777 (sys-stat->mode s))
                       (sys-stat->file-type s))))))))
  )

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
           (close-input-port in)
           (close-output-port out)
           r)))

(test* "pipe and read-block(line)" 2
       (receive (in out) (sys-pipe :buffering :line)
         (display "a\n" out)
         (let1 r (string-size (read-block 1000 in))
           (close-input-port in)
           (close-output-port out)
           r)))

;;-------------------------------------------------------------------
(test-section "fork&exec")

(define (nap)
  (cond-expand
   (gauche.sys.nanosleep (sys-nanosleep 200000000))  ;0.2s
   (else (sys-sleep 1))))

(cond-expand
 [(and (not gauche.os.windows)  ;; win32 doesn't support fork at all.
       (not gauche.os.cygwin))  ;; cygwin's fork is not reliable.
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
               (sys-kill pid SIGKILL)
               (receive (rpid code) (sys-wait)
                 (and (= rpid pid)
                      (sys-wait-signaled? code)
                      (= (sys-wait-termsig code) SIGKILL)))))))

  (test* "fork, wait, kill & sleep" #t
         (let1 pid (sys-fork)
           (if (= pid 0)
             (begin (nap) (sys-exit 0))
             (begin 
               (sys-kill pid SIGSTOP) 
               (receive (rpid code) (sys-waitpid pid :untraced #t)
                 (and (= rpid pid)
                      (sys-wait-stopped? code)
                      (= (sys-wait-stopsig code) SIGSTOP)
                      (begin (sys-kill pid SIGCONT)
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
                          (^e (sys-exit 0))
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

  (test* "fork, exec and signal mask" #t
         (let ((nmask (make <sys-sigset>))
               (cmask (make <sys-sigset>)))
           (sys-sigset-fill! nmask)
           (let ((omask (sys-sigmask SIG_SETMASK nmask))
                 (zero  (open-input-file "/dev/zero")))
             (receive (in out) (sys-pipe :buffering :none)
               (let1 pid
                   (sys-fork-and-exec "cat" '("cat")
                                      :iomap `((0 . ,zero) (1 . ,out))
                                      :sigmask cmask)
                 (read-byte in) ;; make sure 'cat' is started
                 (sys-kill pid SIGINT)
                 (sys-sigmask SIG_SETMASK omask)
                 (sys-waitpid pid)
                 #t)))))

  ;; Testing fork&exec and detached process
  ;; NB: these tests assume we're running the testing gosh in the
  ;; current directory.
  (when (file-exists? "./gosh")
    (cmd-rmrf "test.out")
    ;; The child process.  Gets the expected ppid in the first arg.
    ;; If :detached is true, we expect ppid to be 1.  However, the
    ;; immediate parent may take time to exit, so we loop to synchronize.
    (with-output-to-file "test.out"
      (lambda ()
        (write '(define (main args)
                  (let1 expected-ppid (x->integer (cadr args))
                    (let loop ([c 0])
                      (sys-nanosleep #e2e8)
                      (cond [(or (eqv? (sys-getppid) expected-ppid)
                                 (>= c 50)) ; takes about 10s
                             (write (list (sys-getpid) (sys-getpgrp)))]
                            [else (loop (+ c 1))])))))))
    (define (run-and-read-test msg ppid detached)
      (test* msg detached
             (receive (in out) (sys-pipe :buffering :none)
               (let1 pid (sys-fork-and-exec "./gosh"
                                            `("./gosh" "-ftest" "./test.out"
                                              ,(x->string ppid))
                                            :iomap `((1 . ,out))
                                            :detached detached)
                 (close-port out)
                 (let1 result (read in)
                   (sys-waitpid pid)
                   (eqv? (car result) (cadr result)))))))
    (run-and-read-test "fork, exec and detached process (not detached)" 
                       (sys-getpid) #f)
    (run-and-read-test "fork, exec and detached process (detached)"
                       1 #t))
  (cmd-rmrf "test.out")

  ;; Testing GC in forked process---we don't explicitly spawn a thread here,
  ;; but some architecture (OSX 10.7.3, at least) seems to create a thread
  ;; implicitly.  In the child process all threads are gone except one,
  ;; and GC needs to recognize that.  The following test fails on OSX 10.7.3
  ;; without HANDLE_FORK defined in gc.
  ;; NB: HANDLE_FORK doesn't seem to work on OSX 10.7.4.  Disabling for now.
  ;; (test* "GC in forked process" "done"
  ;;        (receive (in out) (sys-pipe)
  ;;          (let ([pid (sys-fork)])
  ;;            (if (= pid 0)
  ;;              (begin
  ;;                (dotimes (i 1000)
  ;;                  (make-vector 10000))
  ;;                (gc)
  ;;                (display "done\n" out)
  ;;                (sys-exit 0))
  ;;              (let ((line (read-line in)))
  ;;                (sys-waitpid pid)
  ;;                line)))))
  
  ] ; !gauche.os.windows
 [else])

;;-------------------------------------------------------------------
(test-section "select")

(cond-expand
 [gauche.sys.select
  (test* "fdset" '(3 #t #f #t #t #f)
         (let ((fdset (make <sys-fdset>)))
           (set! (sys-fdset-ref fdset (current-input-port)) #t)
           (sys-fdset-set! fdset (current-error-port) #t)
           (sys-fdset-set! fdset 3 #t)
           (sys-fdset-set! fdset 4 #f)
           (cons (sys-fdset-max-fd fdset)
                 (map (^i (sys-fdset-ref fdset i)) (iota 5)))))

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

  (test* "sys-fdset" `(,(port-file-number (current-input-port)) 9 10)
         (sys-fdset->list (sys-fdset 9 (current-input-port) 10)))
  (test* "list->sys-fdset" '(1 3 5 7 9)
         (sys-fdset->list (list->sys-fdset (list (sys-fdset 3 9)
                                                 7
                                                 (sys-fdset 1 3 5)))))
  (test* "sys-fdset-copy!" '(2 4 5)
         (let1 dst (make <sys-fdset>)
           (sys-fdset-copy! dst (sys-fdset 5 4 2))
           (sys-fdset->list dst)))

  (test* "sys-fdset-clear!" '()
         (sys-fdset->list (sys-fdset-clear! (sys-fdset 1 2 3))))

  ;; NB: Windows' select() can't select on non-socket fds, so we skip
  ;; this test.

  (cond-expand
   [(not gauche.os.windows)
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
           )]
   [else]) ; cond-expand (not gauche.os.windows)
  ]
 [else]) ; cond-expand gauche.sys.select

;;-------------------------------------------------------------------
(test-section "signal handling")

(cond-expand
 [(not gauche.os.windows)

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

  (test* "sigalrm3" (test-error)
         (call/cc
          (lambda (k)
            (with-signal-handlers
             ((SIGINT => k)
              (SIGUSR1 => k))
             (lambda ()
               (sys-alarm 1)
               (sys-pause))))))

  (test* "sigalrm4 (interrupting syscall)" SIGALRM
         (call/cc
          (lambda (k)
            (with-signal-handlers
             ((SIGALRM => k))
             (lambda ()
               (receive (in out) (sys-pipe)
                 (sys-alarm 1)
                 (read in)))))))

  (test* "sigalrm5 (interrupting syscall - restart)" '(a)
         (receive (in out) (sys-pipe)
           (with-signal-handlers
            ((SIGALRM (write '(a) out) (flush out)))
            (lambda ()
              (sys-alarm 1)
              (read in)))))

  (when (global-variable-bound? 'gauche 'sys-select)
    (test* "sigalrm6 (interrupting syscall - restart)" '(#t 0)
           (let1 r #f
             (with-signal-handlers
              ((SIGALRM (set! r #t)))
              (lambda ()
                (sys-alarm 1)
                (let1 s (sys-select #f #f #f 1500000)
                  (list r s))))))
    )

  (test* "fork & sigint" #t
         (let ((pid (sys-fork))
               (sigint  #f)
               (sigchld #f))
           (if (= pid 0)
             (let ((parent (sys-getppid)))
               (nap)
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

  ;; NB: on cygwin (as of 1.5.25), sigmask doesn't work reliably so
  ;; we skip test for now.
  (cond-expand
   [(not gauche.os.cygwin)
    (test* "sigmask" 'hup
           (let ((sig #f)
                 (chld #f)
                 (mask1 (sys-sigset SIGINT)))
             (call/cc
              (lambda (k)
                (set-signal-handler! SIGINT  k)
                (set-signal-handler! SIGCHLD (^k (sys-wait) (set! chld #t)))
                (set-signal-handler! SIGHUP  (^k (set! sig 'hup)))
                (sys-sigmask SIG_BLOCK mask1)
                (let ((pid (sys-fork)))
                  (if (= pid 0)
                    (begin
                      (sys-kill (sys-getppid) SIGINT)
                      (nap) ;; solaris seems to lose SIGHUP without this
                      (sys-kill (sys-getppid) SIGHUP)
                      (sys-exit 0))
                    (begin
                      (let loop ()
                        (nap)
                        (unless sig (loop)))
                      (set-signal-handler! SIGINT #f)
                      (sys-sigmask SIG_UNBLOCK mask1)
                      ;;Some systems appear to lose this SIGCHLD (esp. cygwin)
                      ;;(let loop ()
                      ;;  (unless chld (loop)))
                      sig)))))))

    (let ()
      (define (test-double-signal signals mask fire-sig)
        (let ((flag #f)
              (count 0))
          (let/cc break
            (set-signal-handler!
             signals
             (lambda (n)
               (unless flag
                 (inc! count)
                 (when (> count 1) (break 'boo)) ;; avoid infinite reentrance
                 (sys-kill (sys-getpid) fire-sig)
                 (set! flag #t)))
             mask)
            (sys-kill (sys-getpid) SIGHUP)
            flag)))

      (test* "sigmask during interrupt handler (default)" #t
             (test-double-signal SIGHUP #f SIGHUP))

      (test* "sigmask during interrupt handler (explicit)" #t
             (test-double-signal SIGHUP (sys-sigset SIGHUP) SIGHUP))

      (test* "sigmask during interrupt handler (multi/default)" #t
             (test-double-signal (sys-sigset SIGHUP SIGINT)
                                 #f SIGINT))

      (test* "sigmask during interrupt handler (multi/explicit)" #t
             (test-double-signal (sys-sigset SIGHUP SIGINT)
                                 (sys-sigset SIGINT) SIGINT))

      (test* "sigmask during interrupt handler (reentrance)" 'boo
             (test-double-signal SIGHUP (sys-sigset) SIGHUP))

      (test* "sigmask during interrupt handler (multi/reentrance)" 'boo
             (test-double-signal (sys-sigset SIGHUP SIGINT)
                                 (sys-sigset) SIGHUP))

      (set-signal-handler! SIGINT #f)
      )]
   [else]);; !cygwin

  ;; sys-sigwait
  (cond-expand
   [(and gauche.sys.sigwait
         (not gauche.os.cygwin))
    (let ()
      (define z (^n (raise 'foo)))
      
      (set-signal-handler! SIGCHLD #t)
      (set-signal-handler! SIGINT z)
      
      (test* "sys-sigwait" SIGHUP
             (receive (in out) (sys-pipe)
               (let1 pid (sys-fork)
                 (cond ((= pid 0)
                        (close-output-port out)
                        ;; synchronize with parent process
                        (read-char in)
                        (sys-kill (sys-getppid) SIGHUP)
                        (sys-exit 0))
                       (else
                        (close-input-port in)
                        (let* ((sigset (sys-sigset SIGHUP SIGINT))
                               (oldmask (sys-sigmask SIG_BLOCK sigset)))
                          ;; close the pipe to synchronize with child process
                          (close-output-port out)
                          (let1 signo (sys-sigwait sigset)
                            (sys-waitpid pid)
                            (sys-sigmask SIG_SETMASK oldmask)
                            signo)))))))

      (test* "sys-sigwait / signal handler restoration" 'foo
             (guard (e (else e))
               (sys-kill (sys-getpid) SIGINT))))
    ]
   [else]) ; (not (and gauche.sys.sigwait (not gauche.os.cygwin)))

  ] 
 [else]) ; gauche.os.windows

(test-end)

