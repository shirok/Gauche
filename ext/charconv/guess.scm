;;;
;;; Auxiliary script to generate japanese code guessing table
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: guess.scm,v 1.1 2002-06-11 14:07:08 shirok Exp $
;;;

(use srfi-1)
(use gauche.sequence)

;; This is a simple state machine compiler.
;;
;; <state-machine> : (define-dfa <name> <state> ...)
;; <state> : (<name> (<input-set> <next-state> <score>) ...)
;; <name>  : symbol
;; <next-state> : symbol
;; <score> : real
;; <input-set> : (<byte-or-range> ...)
;; <byte-or-range> : <byte> | (<byte> <byte>)
;;
;; When evaluated, the DFA generates a state transition table in
;; C source format.

(define-class <dfa> ()
  ((name    :init-keyword :name   :accessor name-of)
   (states  :init-keyword :states :accessor states-of)
   (instances :allocation :class  :init-value '())))

(define-class <state> ()
  ((name      :init-keyword :name      :accessor name-of)
   (index     :init-keyword :index     :accessor index-of)
   (branches  :init-keyword :branches  :getter branches-of :init-value '())))

(define-class <branch> ()
  ((from-state :init-keyword :from-state :accessor from-state-of)
   (to-state   :init-keyword :to-state   :accessor to-state-of)
   (ranges     :init-keyword :ranges     :accessor ranges-of)
   (index      :init-keyword :index      :accessor index-of)
   (score      :init-keyword :score      :accessor score-of)))

;; Create DFA

(define-syntax define-dfa
  (syntax-rules ()
    ((_ name . states)
     (define name (make <dfa>
                    :name 'name
                    :states (resolve-states 'states))))))

(define-method initialize ((self <dfa>) initargs)
  (next-method)
  (slot-push! self 'instances self))

(define (all-dfas) (reverse (class-slot-ref <dfa> 'instances)))

(define (resolve-states state-defs)
  (let ((states (map (lambda (d i) (make <state> :name (car d) :index i))
                     state-defs
                     (iota (length state-defs)))))
    (fold (lambda (s d i)
            (let1 num-branches (length (cdr d))
              (set! (ref s 'branches)
                    (map (lambda (branch bindex)
                           (make <branch>
                             :from-state s
                             :to-state (or (find (lambda (e)
                                                   (eq? (name-of e) (cadr branch)))
                                                 states)
                                           (error "no such state" (cadr branch)))
                             :ranges (car branch)
                             :index bindex
                             :score (caddr branch)))
                         (cdr d)
                         (iota num-branches i)))
              (+ i num-branches)))
          0
          states state-defs)
    states))

;; Emit state table
(define (emit-dfa-table dfa)
  (format #t "static unsigned char guess_st_~a[][256] = {\n" (name-of dfa))
  (for-each emit-state-table (states-of dfa))
  (print "}\n")
  (format #t "static guess_branch guess_br_~a[] = {\n" (name-of dfa))
  (for-each emit-branch-table
            (append-map branches-of (states-of dfa)))
  (print "}\n")
  )

(define (emit-state-table state)
  (let1 branch-vec (make-vector 256 -1)
    (dolist (br (branches-of state))
      (dolist (range (ranges-of br))
        (if (pair? range)
            (vector-fill! branch-vec (index-of br) (car range) (+ (cadr range) 1))
            (set! (ref branch-vec range) (index-of br)))))
    (format #t " { /* state ~a */" (name-of state))
    (dotimes (i 256)
      (when (zero? (modulo i 16)) (newline))
      (format #t " ~2d," (ref branch-vec i)))
    (print "\n },")
    ))

(define (emit-branch-table branch)
  (format #t " { ~2d, ~5s }, /* ~a -> ~a */\n"
          (index-of (to-state-of branch))
          (score-of branch)
          (name-of (from-state-of branch))
          (name-of (to-state-of branch))))
;;
;; main
;;

(define (main args)
  (for-each emit-dfa-table (all-dfas)))

;;;
;;; EUC-JP
;;;

(define-dfa eucj
  ;; first byte
  (init
   (((#x00 #x7f)) init         1.0)   ; ASCII range
   ((#x8e)        jis0201_kana 0.8)   ; JISX 0201 kana
   ((#x8f)        jis0213_2    0.95)  ; JISX 0213 plane 2
   (((#xa1 #xfe)) jis0213_1    1.0)   ; JISX 0213 plane 1
   )
  ;; jis x 0201 kana
  (jis0201_kana
   (((#xa1 #xdf)) init         1.0)
   )
  ;; jis x 0208 and jis x 0213 plane 1
  (jis0213_1
   (((#xa1 #xfe)) init         1.0))
  ;; jis x 0213 plane 2
  (jis0213_2
   (((#xa1 #xfe)) init         1.0))
  )

;;;
;;; Shift_JIS
;;;

(define-dfa sjis
  ;; first byte
  (init
   (((#x00 #x7f)) init         1.0)     ;ascii
   (((#x81 #x9f) (#xe1 #xef)) jis0213 1.0) ;jisx0213 plane 1
   (((#xa1 #xdf)) init         0.8)     ;jisx0201 kana
   (((#xf0 #xfc)) jis0213      0.95)    ;jisx0213 plane 2
   (((#xfd #xff)) init         0.8))    ;vendor extension
  (jis0213
   (((#x40 #x7e) (#x80 #xfc)) init 1.0))
  )

;;;
;;; UTF-8
;;;

(define-dfa utf8
  (init
   (((#x00 #x7f)) init         1.0)
   (((#xc2 #xdf)) 1byte_more   1.0)
   (((#xe0 #xef)) 2byte_more   1.0)
   (((#xf0 #xf7)) 3byte_more   1.0)
   (((#xf8 #xfb)) 4byte_more   1.0)
   (((#xfc #xfd)) 5byte_more   1.0))
  (1byte_more
   (((#x80 #xbf)) init         1.0))
  (2byte_more
   (((#x80 #xbf)) 1byte_more   1.0))
  (3byte_more
   (((#x80 #xbf)) 2byte_more   1.0))
  (4byte_more
   (((#x80 #xbf)) 3byte_more   1.0))
  (5byte_more
   (((#x80 #xbf)) 4byte_more   1.0))
  )
