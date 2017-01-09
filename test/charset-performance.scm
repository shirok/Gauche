;;
;; testing mutable and immutable char-set lookup speed
;;

(use gauche.time)
(use data.random)
(use gauche.sequence)
(use srfi-1)
(use srfi-14)
(use srfi-42)

(define *cats* '(Lu Ll Lt Lm Lo Mn Nd Nl No Po Sm Zs Cc Co))

;; Returns a map 
(define (setup-char-sets)
  (let1 vec (vector-tabulate (length *cats*) (^_ (char-set)))
    (do-ec (: c 0 #x20000)
           (let1 ch (ucs->char c)
             (if-let1 j (list-index (cute eq? <> (char-general-category ch))
                                    *cats*)
               (char-set-adjoin! (~ vec j) ch))))
    (map (^[cs] (cons cs (char-set-freeze cs))) vec)))

(define (setup-input-data)
  (take (generator->lseq (chars$ #[\x0000;-\x1ffff;])) 2000000))

(define (runner-1 cs input)
  (^[] (dolist [i input] (char-set-contains? cs i))))

;; returns (cat <time-result-mutable> <time-result-immutable>)
(define (run-benchmark csss input)
  ($ map (^x (list (caar x) (cdar x) (cdadr x)))
     $ (cut slices <> 2)
     $ cdr $ time-these 1000
     $ append-map (^[cat css]
                    `((,cat . ,(runner-1 (car css) input))
                      (,#"i~cat" . ,(runner-1 (cdr css) input))))
     *cats* csss))

(define (report-benchmark res)
  (define (row title ta tb)
    (format #t "~6a  ~8@a ~8@a ~8@a    ~8@a ~8@a ~8@a    ~8@a\n"
            title
            (fmt (time-result-real ta))
            (fmt (time-result-user ta))
            (fmt (time-result-sys ta))
            (fmt (time-result-real tb))
            (fmt (time-result-user tb))
            (fmt (time-result-sys tb))
            (fmt (* 100 (/ (time-result-user ta) (time-result-user tb))))))
  (format #t "                    mutable                     immutable\n")
  (format #t "category    real     user      sys        real     user      sys  speed ratio\n")
  (dolist [x res] (apply row x)))

(define (report-charset-stats charsets)
  (define (cs-chunks cs)
    (length (filter (^p (>= (cdr p) #x80))
                    ((with-module gauche.internal %char-set-ranges) cs))))
  (define (cs-report-1 category css)
    (format #t "~6a  ~4@a ranges, ~6@a chars\n"
            category (cs-chunks (car css)) (char-set-size (car css))))
  (for-each cs-report-1 *cats* charsets))

(define (fmt num)
  (let* ([integral (floor->exact num)]
         [fraction (- num integral)]
         [base (* fraction 1000)]
         [base1 (floor->exact base)]
         [bfraction (- base base1)])
    (when (>= bfraction 0.5) (inc! base1))
    (when (>= base1 1000) (inc! integral) (dec! base1 1000))
    (format "~d.~3,'0d" integral base1)))

(define (main args)
  (let1 charsets (setup-char-sets)  ; ((<mutable> . <immutable>) ...)
    (report-benchmark (run-benchmark charsets (setup-input-data)))
    (print)
    (report-charset-stats charsets)))


                        
                     


