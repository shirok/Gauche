;;
;; test for system related procedures
;;

(use gauche.test)
(use srfi-13)

(test-start "system")

;;-------------------------------------------------------------------
(test-section "system")

(test "system" #t (lambda () (sys-system ":") #t))
(test "system" #t (lambda () (sys-system "") #t))

;;-------------------------------------------------------------------
(test-section "filesystem")

(sys-system "rm -rf test.dir >/dev/null")

(test "access" '(#f #f #f #f)
      (lambda ()
        (map (lambda (flag) (sys-access "test.dir" flag))
             (list f_ok r_ok w_ok x_ok))))
(sys-system "touch test.dir")
(sys-system "chmod 777 test.dir")
(test "access" '(#t #t #t #t)
      (lambda ()
        (map (lambda (flag) (sys-access "test.dir" flag))
             (list f_ok r_ok w_ok x_ok))))
(sys-system "chmod 500 test.dir")
(test "access" '(#t #t #f #t)
      (lambda ()
        (map (lambda (flag) (sys-access "test.dir" flag))
             (list f_ok r_ok w_ok x_ok))))
(sys-system "chmod 477 test.dir")
(test "access" '(#t #t #f #f)
      (lambda ()
        (map (lambda (flag) (sys-access "test.dir" flag))
             (list f_ok r_ok w_ok x_ok))))
(sys-system "chmod 000 test.dir")
(test "access" '(#t #f #f #f)
      (lambda ()
        (map (lambda (flag) (sys-access "test.dir" flag))
             (list f_ok r_ok w_ok x_ok))))

(test "unlink" #f
      (lambda ()
        (sys-unlink "test.dir") (sys-access "test.dir" f_ok)))

(define (get-lsmode file)
  (when (sys-access "test.dir/lsout" f_ok)
    (sys-unlink "test.dir/lsout"))
  (sys-system (format #f "ls -ld ~a > test.dir/lsout" file))
  (with-input-from-file "test.dir/lsout"
    (lambda () (string-take (read-line) 10))))

(test "mkdir" "drwxr-x---"
      (lambda ()
        (sys-mkdir "test.dir" #o750)
        (get-lsmode "test.dir")))

(test "chmod" "drwxr-xr-x"
      (lambda ()
        (sys-chmod "test.dir" #o755)
        (get-lsmode "test.dir")))

(with-output-to-file "test.dir/xyzzy"
  (lambda () (display "zzzzzZzzzzZZzZzzzzzzzZzzZZZZz") (newline)))

(test "rename" '(#f #t)
      (lambda ()
        (sys-rename "test.dir/xyzzy" "test.dir/zzZzz")
        (list (sys-access "test.dir/xyzzy" f_ok)
              (sys-access "test.dir/zzZzz" f_ok))))

(test "readdir" '("." ".." "lsout" "zzZzz")
      (lambda ()
        (sort (sys-readdir "test.dir"))))

(test "rmdir" #f
      (lambda ()
        (sys-unlink "test.dir/lsout")
        (sys-unlink "test.dir/zzZzz")
        (sys-rmdir "test.dir")
        (sys-access "test.dir" f_ok)))

;;-------------------------------------------------------------------
(test-section "fork&exec")

(test "fork & wait" #t
      (lambda ()
        (let ((pid (sys-fork)))
          (if (= pid 0)
              (exit 5)
              (let ((status (sys-wait)))
                (and (= (car status) pid)
                     (sys-wait-exited? (cdr status))
                     (= (sys-wait-exit-status (cdr status)) 5)))))))

(test "fork & waitpid" #t
      (lambda ()
        (let ((pid (sys-fork)))
          (if (= pid 0)
              (exit 10)
              (let ((status (sys-waitpid pid)))
                (and (= (car status) pid)
                     (sys-wait-exited? (cdr status))
                     (= (sys-wait-exit-status (cdr status)) 10)))))))

(test "fork, wait & kill" #t
      (lambda ()
        (let ((pid (sys-fork)))
          (if (= pid 0)
              (sys-pause)
              (begin 
                (sys-kill pid SIGINT)
                (let ((status (sys-wait)))
                  (and (= (car status) pid)
                       (sys-wait-signaled? (cdr status))
                       (= (sys-wait-termsig (cdr status)) SIGINT))))))))

(test "fork, wait, kill & sleep" #t
      (lambda ()
        (let ((pid (sys-fork)))
          (if (= pid 0)
              (begin (sys-sleep 1) (exit 0))
              (begin 
                (sys-kill pid SIGSTOP) 
                (let ((status (sys-waitpid pid :untraced #t)))
                  (and (= (car status) pid)
                       (sys-wait-stopped? (cdr status))
                       (= (sys-wait-stopsig (cdr status)) SIGSTOP)
                       (begin (sys-kill pid SIGCONT)
                              (let ((status (sys-wait)))
                                (and (= (car status) pid)
                                     (sys-wait-exited? (cdr status))
                                     (= (sys-wait-exit-status (cdr status)) 0)
                                     )))
                       )))
              )))
      )


;;-------------------------------------------------------------------

(test-end)
