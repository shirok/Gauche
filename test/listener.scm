;;
;; test for listener
;;

(use gauche.test)

(test-start "listener")

(use gauche.listener)
(test-module 'gauche.listener)

(test-section "complete-sexp?")

(define-syntax sexp-tester
  (syntax-rules ()
    [(_ result str)
     (test* (format #f "complete-sexp? ~,,,,40:a" str)
            result
            (complete-sexp? str))]
    ))

(sexp-tester #t "")
(sexp-tester #t "a")
(sexp-tester #t "abc")
(sexp-tester #t "123")
(sexp-tester #t "  3/4  ")
(sexp-tester #t "  3/4")
(sexp-tester #t "()")
(sexp-tester #t "(abc)")
(sexp-tester #t " ( a ) ")
(sexp-tester #t " (a) ")
(sexp-tester #t "(a . b)")
(sexp-tester #t " ((((a)))) ")
(sexp-tester #f " ((((a))) ")
(sexp-tester #f " (((( a ))) ")
(sexp-tester #t "(ab cd ef (guhr janr) ((airugn jenr) (bjn unrg)) () )")
(sexp-tester #t "(ab cd ef [guhr janr] {[airugn jenr] (bjn unrg)} () )")
(sexp-tester #f "(ab cd ef [guhr janr] {[airugn jenr} (bjn unrg)] () )")
(sexp-tester #t " \"rugier\"")
(sexp-tester #t " \"rugi \\\"er\\\" unga\"")
(sexp-tester #t " \"\\\"\\\"\"")
(sexp-tester #f " \"\\\"er\\\"")
(sexp-tester #t " \"\\\"er\"")
(sexp-tester #t " \"\\\"(\"")
(sexp-tester #t "#\\a")
(sexp-tester #f "#\\")
(sexp-tester #t "#\\abunaga")
(sexp-tester #t "#\\abunaga'(boogaz)")
(sexp-tester #f "#\\abunaga'(boogaz")
(sexp-tester #t "#\\(")
(sexp-tester #t "(#\\( )")
(sexp-tester #t "(#\\(gunar)")
(sexp-tester #t "(#\\(gunar)")
(sexp-tester #t "#(bunga bunga)")
(sexp-tester #t "[#(bunga bunga)]")
(sexp-tester #t "#x#d3242(bunar)")
(sexp-tester #t "|buna(-|")
(sexp-tester #f "|buna(-")
(sexp-tester #t "|buna(-\\|zuppe|")
(sexp-tester #t "|buna(-\\|zu[p\"e|")
(sexp-tester #t "(|buna(-| . a)")
(sexp-tester #t "#,(bunga bunga bunga)")
(sexp-tester #t "#,()")
(sexp-tester #f "#,(yop")
(sexp-tester #t "(#,( () ) . a)")
(sexp-tester #t "#[a-z]")
(sexp-tester #t "#[[:alpha:]]")
(sexp-tester #t "#[\\]]")
(sexp-tester #f "#[1234")
(sexp-tester #f "(#[1234 . )")
(sexp-tester #t "(#[1234] . a)")
(sexp-tester #t "[#[1234] . a]")
(sexp-tester #t "#/reg(exp)fofofo[\\s\\d]/")
(sexp-tester #t "#/(/")
(sexp-tester #t "#/\\(/")
(sexp-tester #t "#/\\/usr\\/bin/")
(sexp-tester #f "#/\\/usr\\/bin  ")
(sexp-tester #t "(#/(/ . a)")
(sexp-tester (test-error) "(ibanr #<booba> )")

(test-section "listener")

(define-values (ipipe-in ipipe-out) (sys-pipe))
(define-values (opipe-in opipe-out) (sys-pipe))
(define-values (epipe-in epipe-out) (sys-pipe))

(set! (port-buffering ipipe-in) :none)
(set! (port-buffering ipipe-out) :none)
(set! (port-buffering opipe-in) :none)
(set! (port-buffering opipe-out) :none)

(define *fatal* #f)

(define (fatal x) (set! *fatal* x) #t)

(define listener
  (make <listener>
    :input-port ipipe-in
    :output-port opipe-out
    :error-port epipe-out
    :prompter (^[] (display "<<<\n"))
    :fatal-handler fatal))

(define handler (listener-read-handler listener))

(test* "prompter" "<<<"
       (begin
         (listener-show-prompt listener)
         (read-line opipe-in)))

(define (send-expr expr)
  (display expr ipipe-out) (flush ipipe-out))

(define (read-results)
  (let loop ([l (read-line opipe-in)]
             [r '()])
    (if (equal? l "<<<")
        (reverse r)
        (loop (read-line opipe-in) (cons l r)))))

;; NB: at this moment, the tests don't work on windows because of
;; some buffering weirdness.  We omit tests on them.
(cond-expand
 (gauche.os.windows)
 (else

(test* "listener" '("3")
       (begin
         (send-expr "(+ 1 2)\n")
         (handler)
         (read-results)))

(test* "listener" '("1" "2" "3")
       (begin
         (send-expr "(values 1 2 3)\n")
         (handler)
         (read-results)))

(test* "listener" '(("1") ("2"))
       (begin
         (send-expr "1 2\n")
         (handler)
         (let* ([r0 (read-results)]
                [r1 (read-results)])
           (list r0 r1))))

(test* "listener" '("3")
       (begin
         (send-expr "(+ 1 \n")
         (handler)
         (send-expr "2")
         (handler)
         (send-expr ")")
         (handler)
         (read-results)))

(test* "listener" '(("#\\a") ("3"))
       (begin
         (send-expr "#\\")
         (handler)
         (send-expr "a (+")
         (handler)
         (send-expr " 1 2)")
         (handler)
         (let* ([r0 (read-results)]
                [r1 (read-results)])
           (list r0 r1))))

;(test* "listener (error)" "*** ERROR: unbound variable: zzz"
;       (begin
;         (send-expr "zzz")
;         (handler)
;         (read-line epipe-in)))

(test* "listener (fatal error)" <system-error>
       (begin
         (close-input-port opipe-in)
         (send-expr "(+ 1 2)")
         (handler)
         (class-of *fatal*)))

)) ;; (not gauche.os.windows)

(test-end)
