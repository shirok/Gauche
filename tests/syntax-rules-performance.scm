;;;
;;; The following code is from Oleg Kiselyov's syntax-rule-stress-test,
;;;
;;; http://okmij.org/ftp/Scheme/macros.html#syntax-rule-stress-test
;;;
;;; The code is put in public domain by the author ( http://okmij.org/ftp/ )
;;;

; A stress test of the syntax-rule macro-expander.
; The following code, when evaluated, prints if number 5 is prime.
; The code implements the sieve of Eratosthenes, (see the macro ?sieve)
; The code is generated automatically by the Scheme-to-syntax-rule
; compiler.
; See macros.html in this directory for more detail.

(define-syntax ??!apply (syntax-rules (??!lambda) ((_ (??!lambda (bound-var . other-bound-vars) body) oval . other-ovals) (letrec-syntax ((subs (syntax-rules (??! bound-var ??!lambda) ((_ val k (??! bound-var)) (appl k val)) ((_ val k (??!lambda bvars int-body)) (subs-in-lambda val bvars (k bvars) int-body)) ((_ val k (x)) (subs val (recon-pair val k ()) x)) ((_ val k (x . y)) (subs val (subsed-cdr val k x) y)) ((_ val k x) (appl k x)))) (subsed-cdr (syntax-rules () ((_ val k x new-y) (subs val (recon-pair val k new-y) x)))) (recon-pair (syntax-rules () ((_ val k new-y new-x) (appl k (new-x . new-y))))) (subs-in-lambda (syntax-rules (bound-var) ((_ val () kp int-body) (subs val (recon-l kp ()) int-body)) ((_ val (bound-var . obvars) (k bvars) int-body) (appl k (??!lambda bvars int-body))) ((_ val (obvar . obvars) kp int-body) (subs-in-lambda val obvars kp int-body)))) (recon-l (syntax-rules () ((_ (k bvars) () result) (appl k (??!lambda bvars result))))) (appl (syntax-rules () ((_ (a b c d) result) (a b c d result)) ((_ (a b c) result) (a b c result)))) (finish (syntax-rules () ((_ () () exp) exp) ((_ rem-bvars rem-ovals exps) (??!apply (??!lambda rem-bvars exps) . rem-ovals))))) (subs oval (finish other-bound-vars other-ovals) body)))))
(define-syntax ?car (syntax-rules () ((_ (x . y) k) (??!apply k x))))
(define-syntax ?cdr (syntax-rules () ((_ (x . y) k) (??!apply k y))))
(define-syntax ?cons (syntax-rules () ((_ x y k) (??!apply k (x . y)))))
(define-syntax ?null? (syntax-rules () ((_ () k) (??!apply k #t)) ((_ x k) (??!apply k #f))))
(define-syntax ?ifnull? (syntax-rules () ((_ () kt kf) (??!apply kt #t)) ((_ x kt kf) (??!apply kf #f))))
(define-syntax ?pair? (syntax-rules () ((_ (a . b) k) (??!apply k #t)) ((_ not-pair k) (??!apply k #f))))
(define-syntax ?ifpair? (syntax-rules () ((_ (a . b) kt kf) (??!apply kt #t)) ((_ not-pair kt kf) (??!apply kf #f))))
(define-syntax ?true? (syntax-rules () ((_ x k) (??!apply k x))))
(define-syntax ?iftrue? (syntax-rules () ((_ #f kt kf) (??!apply kf #f)) ((_ x kt kf) (??!apply kt #t))))
(define-syntax ?append (syntax-rules () ((_ (x ...) (y ...) k) (??!apply k (x ... y ...)))))
(define-syntax ?ifeq? (syntax-rules () ((_ (x . y) b kt kf) (??!apply kf #f)) ((_ () b kt kf) (??!apply kf #f)) ((_ a b _kt _kf) (let-syntax ((aux (syntax-rules (a) ((_ a kt kf) (??!apply kt #t)) ((_ other kt kf) (??!apply kf #f))))) (aux b _kt _kf)))))
(define-syntax ?ifmemq? (syntax-rules () ((_ a lst kt kf) (?ifpair? lst (??!lambda (_) (?car lst (??!lambda (x) (?ifeq? a (??! x) (??!lambda (_) (??!apply kt #t)) (??!lambda (_) (?cdr lst (??!lambda (tail) (?ifmemq? a (??! tail) kt kf)))))))) (??!lambda (_) (??!apply kf #f))))))
(define-syntax ?number-zero (syntax-rules () ((_ k) (??!apply k ()))))
(define-syntax ?number-two (syntax-rules () ((_ k) (??!apply k ((()))))))
(define-syntax ?incr (syntax-rules () ((_ n k) (??!apply k (n)))))
(define-syntax ?decr (syntax-rules () ((_ (n) k) (??!apply k n))))
(define-syntax ?less-than-two? (syntax-rules () ((_ ((n)) k) (??!apply k #f)) ((_ x k) (??!apply k #t))))
(define-syntax ?ifless-than-two? (syntax-rules () ((_ ((n)) kt kf) (??!apply kf #f)) ((_ x kt kf) (??!apply kt #t))))
(define-syntax ?number-zero? (syntax-rules () ((_ () k) (??!apply k #t)) ((_ x k) (??!apply k #f))))
(define-syntax ?ifnumber-zero? (syntax-rules () ((_ () kt kf) (??!apply kt #t)) ((_ x kt kf) (??!apply kf #f))))
(define-syntax ?iota (syntax-rules () ((_ _?n _?kg1029) (letrec-syntax ((?loop (syntax-rules () ((_ _?currg1031 _?counterg1032 _?kg1030) (?ifless-than-two? _?counterg1032 (??!lambda (g1033) (??!apply _?kg1030 ())) (??!lambda (g1034) (?incr _?currg1031 (??!lambda (g1036) (?decr _?counterg1032 (??!lambda (g1037) (?loop (??! g1036) (??! g1037) (??!lambda (g1035) (?cons _?currg1031 (??! g1035) _?kg1030))))))))))))) (?number-two (??!lambda (g1038) (?loop (??! g1038) _?n _?kg1029)))))))
(define-syntax ?sieve (syntax-rules () ((_ _?lst _?kg1039) (letrec-syntax ((?choose-pivot (syntax-rules () ((_ _?lstg1041 _?kg1040) (?ifnull? _?lstg1041 (??!lambda (g1042) (??!apply _?kg1040 _?lstg1041)) (??!lambda (g1043) (?car _?lstg1041 (??!lambda (g1057) (?number-zero? (??! g1057) (??!lambda (g1044) (?iftrue? (??! g1044) (??!lambda (g1045) (?car _?lstg1041 (??!lambda (g1046) (?cdr _?lstg1041 (??!lambda (g1048) (?choose-pivot (??! g1048) (??!lambda (g1047) (?cons (??! g1046) (??! g1047) _?kg1040)))))))) (??!lambda (g1049) (?car _?lstg1041 (??!lambda (g1050) (?car _?lstg1041 (??!lambda (g1053) (?car _?lstg1041 (??!lambda (g1056) (?decr (??! g1056) (??!lambda (g1054) (?cdr _?lstg1041 (??!lambda (g1055) (?do-sieve (??! g1053) (??! g1054) (??! g1055) (??!lambda (g1052) (?choose-pivot (??! g1052) (??!lambda (g1051) (?cons (??! g1050) (??! g1051) _?kg1040)))))))))))))))))))))))))) (?do-sieve (syntax-rules () ((_ _?stepg1059 _?currentg1060 _?lstg1061 _?kg1058) (?ifnull? _?lstg1061 (??!lambda (g1062) (??!apply _?kg1058 _?lstg1061)) (??!lambda (g1063) (?ifnumber-zero? _?currentg1060 (??!lambda (g1064) (?number-zero (??!lambda (g1065) (?decr _?stepg1059 (??!lambda (g1067) (?cdr _?lstg1061 (??!lambda (g1068) (?do-sieve _?stepg1059 (??! g1067) (??! g1068) (??!lambda (g1066) (?cons (??! g1065) (??! g1066) _?kg1058)))))))))) (??!lambda (g1069) (?car _?lstg1061 (??!lambda (g1070) (?decr _?currentg1060 (??!lambda (g1072) (?cdr _?lstg1061 (??!lambda (g1073) (?do-sieve _?stepg1059 (??! g1072) (??! g1073) (??!lambda (g1071) (?cons (??! g1070) (??! g1071) _?kg1058))))))))))))))))) (?choose-pivot _?lst _?kg1039)))))
(define-syntax ?is-prime (syntax-rules () ((_ _?n _?kg1074) (?iota _?n (??!lambda (g1081) (?sieve (??! g1081) (??!lambda (g1080) (?reverse (??! g1080) (??!lambda (g1079) (?car (??! g1079) (??!lambda (g1078) (?number-zero? (??! g1078) (??!lambda (g1075) (?iftrue? (??! g1075) (??!lambda (g1076) (??!apply _?kg1074 composite)) (??!lambda (g1077) (??!apply _?kg1074 prime))))))))))))))))
(define-syntax ?reverse (syntax-rules () ((_ _?lst _?kg1082) (letrec-syntax ((?loop (syntax-rules () ((_ _?lstg1084 _?accumg1085 _?kg1083) (?ifnull? _?lstg1084 (??!lambda (g1086) (??!apply _?kg1083 _?accumg1085)) (??!lambda (g1087) (?cdr _?lstg1084 (??!lambda (g1088) (?car _?lstg1084 (??!lambda (g1090) (?cons (??! g1090) _?accumg1085 (??!lambda (g1089) (?loop (??! g1088) (??! g1089) _?kg1083))))))))))))) (?loop _?lst () _?kg1082)))))
(?is-prime (((((()))))) (??!lambda (x) (display (quote (??! x)))))
(newline)
