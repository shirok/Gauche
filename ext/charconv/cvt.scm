;;;
;;; Auxiliary script to generate EUC_JISX0213 <-> Unicode 3.2 table
;;;
;;; Shiro Kawai (shiro@acm.org)
;;;
;;; $Id: cvt.scm,v 1.1 2002-05-29 11:29:17 shirok Exp $

(use srfi-1)
(use srfi-11)

;; Parse the table "euc-jp-2000-std.txt", and returns a list of all characters.
;; Each character is represented as
;;   (EUC-JP-codepoint . UCS4-codepoint)
;; Or
;;   (EUC-JP-codepoint . (UCS4-codepoint . UCS4-combining-character))
(define (parse)
  (port-fold (lambda (line knil)
               (rxmatch-case line
                 (#/^0x([0-9A-F]+)\s+U\+([0-9A-F]+)(\+[0-9A-F]+)?/
                       (#f code uni uni2)
                       (let ((n-code (string->number code 16))
                             (n-uni  (string->number uni 16))
                             (n-uni2 (and uni2 (string->number uni2 16))))
                         (if n-uni2
                             (acons n-code (cons n-uni n-uni2) knil)
                             (acons n-code n-uni knil))))
                 (else knil)))
             '()
             read-line))

(define (ucs4->utf8 code)
  (cond ((< code #x80)
         (list code))
        ((< code #x800)
         (list (logior (logand (ash code -6) #x1f) #xc0)
               (logior (logand code #x3f) #x80)))
        ((< code #x10000)
         (list (logior (logand (ash code -12) #x0f) #xe0)
               (logior (logand (ash code -6)  #x3f) #x80)
               (logior (logand code #x3f) #x80)))
        ((< code #x200000)
         (list (logior (logand (ash code -18) #x07) #xf0)
               (logior (logand (ash code -12) #x3f) #x80)
               (logior (logand (ash code -6) #x3f) #x80)
               (logior (logand code #x3f) #x80)))
        ((< code #x4000000)
         (list (logior (logand (ash code -24) #x03) #xf8)
               (logior (logand (ash code -18) #x3f) #x80)
               (logior (logand (ash code -12) #x3f) #x80)
               (logior (logand (ash code -6) #x3f) #x80)
               (logior (logand code #x3f) #x80)))
        (else
         (list (logior (logand (ash code -30) #x01) #xfc)
               (logior (logand (ash code -24) #x3f) #x80)
               (logior (logand (ash code -18) #x3f) #x80)
               (logior (logand (ash code -12) #x3f) #x80)
               (logior (logand (ash code -6) #x3f) #x80)
               (logior (logand code #x3f) #x80)))
        ))

;; Generates EUC_JP to UCS map table.
;; Map is separated in three parts, JISX0201 KANA, JISX0213 plane 1
;; and JISX0213 plane 2.
;; For JISX0201 KANA, the value of the table is UCS2 value.
;; For JISX0213, the value of the table may be either one of the followings:
;;   (1) UCS4, if the value < 0x30000
;;   (2) Two UCS2 values, otherwise.
;;
(define (generate-eucj->ucs data)

  (define (ucsvalue-of cell)
    (let1 ucs (cdr cell)
      (if (pair? ucs)
          (+ (ash (car ucs) 16) (cdr ucs))
          ucs)))

  ;; JISX 0201 kana region
  (define (jisx0201 data)
    (print "/****** EUC_JP -> UCS2 JISX0201-KANA (0x8e??) ******/")
    (print "/* index = e2 - 0xa1 */")
    (print "static unsigned short euc_jisx0201_to_ucs2[] = {")
    (begin0 (write-data data car cdr #x8ea1 #x8ee0 8)
            (print "};")
            (newline)))
  ;; JISX 0213 plane 1 region
  (define (jisx0213-1 data)
    (print "/****** EUC_JP -> UCS4  JISX0213 plane 1 *******/")
    (print "/* index = (e1 - 0xa1, e2 - 0xa1) */")
    (print "static unsigned int euc_jisx0213_1_to_ucs2[][94] = {")
    (begin0
     (let loop ((e1 #xa1) (data data))
       (if (= e1 #xff)
           data
           (loop (+ e1 1)
                 (begin (print " {")
                        (begin0 (write-data data car ucsvalue-of
                                            (+ (* e1 256) #xa1)
                                            (+ (* e1 256) #xff)
                                            8)
                                (print " },"))))))
     (print "};")
     (newline)))
  ;; JISX 0213 plane 2 region
  ;; tricky: the second byte is one of 0xa1, 0xa3, 0xa4, 0xa5, 0xa8,
  ;; 0xac, 0xad, 0xae, 0xaf, and 0xee - 0xfe.
  (define (jisx0213-2 data)
    (define e1list '(#xa1 #xa3 #xa4 #xa5 #xa8 #xac #xad #xae #xaf
                     #xee #xef #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6
                     #xf7 #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe))
    (print "/****** EUC_JP -> UCS4  JISX0213 plane 2 (0x8f????) *****/")
    (print "/* table to traslate second byte into the first index */")
    (print "static short euc_jisx0213_2_index[] = {")
    (let loop ((e1 #xa1) (count 0) (e1list e1list))
      (cond ((= e1 #xff))
            ((= e1 (car e1list))
             (format #t " ~a," count)
             (loop (+ e1 1) (+ count 1) (cdr e1list)))
            (else
             (format #t " -1,")
             (loop (+ e1 1) count e1list))))
    (print "\n};\n")
    (print "/* index = (e1table, e2 - 0xa1) */")
    (print "static unsigned int euc_jisx0213_2_to_ucs2[][94] = {")
    (let loop ((e1list e1list) (data data))
      (if (null? e1list)
          data
          (loop (cdr e1list)
                (let1 e1v (* (car e1list) 256)
                  (print " {")
                  (begin0 (write-data data car ucsvalue-of
                                      (+ e1v #x8f00a1)
                                      (+ e1v #x8f00ff)
                                      8)
                          (print " },"))))))
    (print "};")
    (newline))

  ;; Body of generate-eucj->ucs 
  (let* ((sorted (sort data  (lambda (a b) (< (car a) (car b)))))
         (data   (jisx0201 sorted))
         (data1  (jisx0213-1 data)))
    (jisx0213-2 data1)
    #f))

(define (write-data data key-of value-of start end unit)
  (let loop ((column -1)
             (data data)
             (next start))
    (cond ((>= next end) data)
          ((>= column unit) (newline) (loop -1 data next))
          ((= column -1)
           (format #t " /* 0x~4,'0x -- 0x~4,'0x */\n"
                   next (min (+ next unit -1) (- end 1)))
           (loop 0 data next))
          ((pair? data)
           (let1 key (key-of (car data))
             (cond ((< key next) (loop column (cdr data) next))
                   ((= key next)
                    (format #t " 0x~4,'0x," (value-of (car data)))
                    (loop (+ column 1) (cdr data) (+ next 1)))
                   (else
                    (format #t " 0x0000,")
                    (loop (+ column 1) data (+ next 1))))))
          (else
           (format #t " 0x0000,")
           (loop (+ column 1) data (+ next 1))))))

;; Generates UCS to EUC_JP table
;; 