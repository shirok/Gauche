;;
;; This program is a straightforward port from the code in
;; "Common Lisp, the Language 2nd Edition", section 12.5.3, pp. 339--349.
;;

(use srfi-1)
(use srfi-13)

(define-syntax defparameter
  (syntax-rules ()
    ((_ var val) (define var val))))

(define-syntax defun
  (syntax-rules ()
    ((_ name args . body) (define name (lambda args . body)))))

(define-syntax ignore-errors
  (syntax-rules ()
    ((_ form) form)))                   ;for now

(define-syntax pop                      ;not correct, but enough for here
  (syntax-rules ()
    ((_ loc) (set! loc (cdr loc)))))

(define-syntax push                     ;not correct, but enough for here
  (syntax-rules ()
    ((_ val loc) (set! loc (cons val loc)))))

(define-syntax dotimes
  (syntax-rules ()
    ((_ (var limit) . body)
     (for-each (lambda (var) . body) (iota limit)))))

(define-syntax dolist
  (syntax-rules ()
    ((_ (var list) . body)
     (for-each (lambda (var) . body) list))))

(define (signum num)
  (if (zero? num) num (/ num (abs num))))

(define rest cdr)

(define (cis radians) (make-polar 1.0 radians))
(define (complex r i) (make-rectangular r i))

;; approximation
(define most-positive-single-float 1.75e308)
(define least-positive-single-float 1e-323)

(define pi 3.141592653589793238462643)

;; Original code
(defparameter units-to-show 4.1)
(defparameter text-width-in-picas 28.0)
(defparameter device-pixels-per-inch 300)
(defparameter pixels-per-unit
  (* (/ (/ text-width-in-picas 6)
        (* units-to-show 2))
     device-pixels-per-inch))

(defparameter big (sqrt (sqrt most-positive-single-float)))
(defparameter tiny (sqrt (sqrt least-positive-single-float)))

(defparameter path-really-losing 1000.0)
(defparameter path-outer-limit (* units-to-show (sqrt 2) 1.1))
(defparameter path-minimal-delta (/ 10 pixels-per-unit))
(defparameter path-outer-delta (* path-outer-limit 0.3))
(defparameter path-relative-closeness 0.00001)
(defparameter back-off-delta 0.0005)

(defun comment-line (stream . stuff)
  (format stream "\n% ")
  (apply format stream stuff)
  ;(format #t "\n% ")
  ;(apply format #t stuff)
  )

(defun parametric-path (from to paramfn plotfn)
  (check-arg positive? from)
  (check-arg positive? to)
  (letrec ((domainval (lambda (x) (paramfn x)))
           (rangeval  (lambda (x) (plotfn (paramfn x))))
           (losing (lambda (x)
                     (or (null? x)
                         (not (= (real-part x) (real-part x)))  ;NaN?
                         (not (= (imag-part x) (imag-part x)))  ;NaN?
                         (> (abs (real-part x)) path-really-losing)
                         (> (abs (imag-part x)) path-really-losing)))))
    (when (> to 1000.0)
      (let ((f0 (rangeval from))
            (f1 (rangeval (+ from 1)))
            (f2 (rangeval (+ from (* 2 pi))))
            (f3 (rangeval (+ from 1 (* 2 pi))))
            (f4 (rangeval (+ from (* 4 pi)))))
        (let ((close
               (lambda (x y)
                 (or (< (careful-abs (- x y)) path-minimal-delta)
                     (< (careful-abs (- x y))
                        (* (+ (careful-abs x) (careful-abs y))
                           path-relative-closeness))))))
          (when (and (close f0 f2)
                     (close f2 f4)
                     (close f1 f3)
                     (or (and (close f0 f1)
                              (close f2 f3))
                         (and (not (close f0 f1))
                              (not (close f2 f3)))))
            (format #t "\nPeriodicity detected.")
            (set! to (+ from (* (signum (- to from)) 2 pi)))))))
     (let ((fromrange (ignore-errors (rangeval from)))
           (torange (ignore-errors (rangeval to))))
      (if (losing fromrange)
          (if (losing torange)
              '()
              (parametric-path (back-off from to) to paramfn plotfn))
          (if (losing torange)
              (parametric-path from (back-off to from) paramfn plotfn)
              (expand-path (refine-path (list from to) rangeval)
                           rangeval))))))

(defun back-off (point other)
  (if (or (> point 10.0) (< point 0.1))
      (let ((sp (sqrt point)))
        (if (or (> point sp other) (< point sp other))
            sp
            (* sp (sqrt other))))
      (+ point (* (signum (- other point)) back-off-delta))))

(defun careful-abs (z)
  (cond ((or (> (real-part z) big)
             (< (real-part z) (- big))
             (> (imag-part z) big)
             (< (imag-part z) (- big)))
         big)
        ((complex? z) (abs z))
        ((negative? z) (- z))
        (t z)))

(defparameter max-refinements 5000)

(defun refine-path (original-path rangeval)
  (call/cc
   (lambda (break)
     (let ((path original-path))
      (do ((j 0 (+ j 1)))
          ((null? (cdr path)))
        (when (zero? (modulo (+ j 1) max-refinements))
          (break "Runaway path"))
        (let* ((from (first path))
               (to (second path))
               (fromrange (rangeval from))
               (torange (rangeval to))
               (dist (careful-abs (- torange fromrange)))
               (mid (* (sqrt from) (sqrt to)))
               (midrange (rangeval mid)))
          (cond ((or (and (far-out fromrange) (far-out torange))
                     (and (< dist path-minimal-delta)
                          (< (abs (- midrange fromrange))
                             path-minimal-delta)
                          ;; Next test is intentionally asymmetric to
                          ;;  avoid problems with periodic functions.
                          (< (abs (- (rangeval (/ (+ to (* from 1.5))
                                                  2.5))
                                     fromrange))
                             path-minimal-delta)))
                 (pop path))
                ((= mid from) (pop path))
                ((= mid to) (pop path))
                (else (set! (cdr path) (cons mid (cdr path))))))))))
   original-path)

(defun expand-path (path rangeval)
  (call/cc
   (lambda (break)
    (let ((final-path (list (rangeval (first path)))))
      (do ((p (rest path) (cdr p)))
          ((null? p)
           (unless (not (null? (rest final-path)))
             (break "Singleton path"))
           (reverse final-path))
        (let ((v (rangeval (car p))))
          (cond ((and (not (null? (rest final-path)))
                      (not (far-out v))
                      (not (far-out (first final-path)))
                      (between v (first final-path)
                                 (second final-path)))
                 (set! (car final-path) v))
                ((null? (rest p))   ;Mustn't omit last point
                 (push v final-path))
                ((< (abs (- v (first final-path))) path-minimal-delta))
                ((far-out v)
                 (unless (and (far-out (first final-path))
                              (< (abs (- v (first final-path)))
                                 path-outer-delta))
                   (push (* 1.01 path-outer-limit (signum v))
                         final-path)))
                (else (push v final-path)))))))))

(defun far-out (x)
  (> (careful-abs x) path-outer-limit))

(defparameter between-tolerance 0.000001)

(defun between (p q r)
  (let ((px (real-part p)) (py (imag-part p))
        (qx (real-part q)) (qy (imag-part q))
        (rx (real-part r)) (ry (imag-part r)))
    (and (or (<= px qx rx) (>= px qx rx))
         (or (<= py qy ry) (>= py qy ry))
         (< (abs (- (* (- qx px) (- ry qy))
                    (* (- rx qx) (- qy py))))
            between-tolerance))))

(defun circle (radius)
  (lambda (angle) (* radius (cis angle))))

(defun hline (imag)
  (lambda (real) (complex real imag)))

(defun vline (real)
  (lambda (imag) (complex real imag)))

(defun -hline (imag)
  (lambda (real) (complex (- real) imag)))

(defun -vline (real)
  (lambda (imag) (complex real (- imag))))

(defun radial (phi quadrant)
  (lambda (rho) (repair-quadrant (* rho (cis phi)) quadrant)))

(defun circumferential (rho quadrant)
  (lambda (phi) (repair-quadrant (* rho (cis phi)) quadrant)))

;;; Quadrant is 0, 1, 2, or 3, meaning I, II, III, or IV.

(defun repair-quadrant (z quadrant)
  (complex (* (+ (abs (real-part z)) tiny)
              (case quadrant ((0) 1.0) ((1) -1.0) ((2) -1.0) ((3) 1.0)))
           (* (+ (abs (imag-part z)) tiny)
              (case quadrant ((0) 1.0) ((1) 1.0) ((2) -1.0) ((3) -1.0)))))

(defun clamp-real (x)
  (if (far-out x)
      (* (signum x) path-outer-limit)
      (round-real x)))

(defun round-real (x)
  (/ (round (* x 10000.0)) 10000.0))

(defun round-point (z)
  (complex (round-real (real-part z)) (round-real (imag-part z))))

(defparameter hiringshade 0.97)
(defparameter loringshade 0.45)

(defparameter ticklength 0.12)
(defparameter smallticklength 0.09)

;;; This determines the pattern of lines and annuli to be drawn.
(defun moby-grid args
  (let-optional* args ((fn sqrt) (stream #t))
    (comment-line stream "Moby grid for function ~S" fn)
    (shaded-annulus 0.25 0.5 4 hiringshade loringshade fn stream)
    (shaded-annulus 0.75 1.0 8 hiringshade loringshade fn stream)
    (shaded-annulus (/ pi 2) 2.0 16 hiringshade loringshade fn stream)
    (shaded-annulus 3 pi 32 hiringshade loringshade fn stream)
    (moby-lines :horizontal 1.0 fn stream)
    (moby-lines :horizontal -1.0 fn stream)
    (moby-lines :vertical 1.0 fn stream)
    (moby-lines :vertical -1.0 fn stream)
    (let ((tickline 0.015)
          (axisline 0.008))
      (letrec ((tick (lambda (n) (straight-line (complex n ticklength)
                                                (complex n (- ticklength))
                                                tickline
                                                stream)))
               (smalltick
                (lambda (n) (straight-line (complex n smallticklength)
                                           (complex n (- smallticklength))
                                           tickline
                                           stream))))
        (comment-line stream "Real axis")
        (straight-line -5 5 axisline stream)
        (dotimes (j (floor units-to-show))
                 (let ((q (+ j 1))) (tick q) (tick (- q))))
        (dotimes (j (floor (/ units-to-show (/ pi 2))))
                 (let ((q (* (/ pi 2) (+ j 1))))
                   (smalltick q)
                   (smalltick (- q)))))
    (letrec ((tick
              (lambda (n) (straight-line (complex ticklength n)
                                         (complex (- ticklength) n)
                                         tickline
                                         stream)))
             (smalltick
              (lambda (n) (straight-line (complex smallticklength n)
                                         (complex (- smallticklength) n)
                                         tickline
                                         stream))))
      (comment-line stream "Imaginary axis")
      (straight-line -5i +5i axisline stream)
      (dotimes (j (floor units-to-show))
        (let ((q (+ j 1))) (tick q) (tick (- q))))
      (dotimes (j (floor (/ units-to-show (/ pi 2))))
        (let ((q (* (/ pi 2) (+ j 1))))
          (smalltick q)
          (smalltick (- q))))))))

(defun straight-line (from to wid stream)
  (format stream
          "\nnewpath  ~S ~S moveto  ~S ~S lineto  ~S setlinewidth  1  setlinecap  stroke"
          (real-part from)
          (imag-part from)
          (real-part to)
          (imag-part to)
          wid))
          
;;; This function draws the lines for the pattern.
(defun moby-lines (orientation signum plotfn stream)
  (let ((paramfn (case orientation
                   ((:horizontal) (if (< signum 0) -hline hline))
                   ((:vertical) (if (< signum 0) -vline vline)))))
    (letrec ((foo
              (lambda (from to other wid)
                (case orientation
                  ((:horizontal)
                   (comment-line stream
                                 "Horizontal line from (~S, ~S) to (~S, ~S)"
                                 (round-real (* signum from))
                                 (round-real other)
                                 (round-real (* signum to))
                                 (round-real other)))
                  ((:vertical)
                   (comment-line stream
                                 "Vertical line from (~S, ~S) to (~S, ~S)"
                                 (round-real other)
                                 (round-real (* signum from))
                                 (round-real other)
                                 (round-real (* signum to)))))
                (postscript-path
                 stream
                 (parametric-path from
                                  to
                                  (paramfn other)
                                  plotfn))
                (postscript-penstroke stream wid))))
      (let* ((thick 0.05)
             (thin 0.02))
        ;; Main axis
        (foo 0.5 tiny 0.0 thick)
        (foo 0.5 1.0 0.0 thick)
        (foo 2.0 1.0 0.0 thick)
        (foo 2.0 big 0.0 thick)
        ;; Parallels at 1 and -1
        (foo 2.0 tiny 1.0 thin)
        (foo 2.0 big 1.0 thin)
        (foo 2.0 tiny -1.0 thin)
        (foo 2.0 big -1.0 thin)
        ;; Parallels at 2, 3, -2, -3
        (foo tiny big 2.0 thin)
        (foo tiny big -2.0 thin)
        (foo tiny big 3.0 thin)
        (foo tiny big -3.0 thin)))))

(defun splice (p q)
  (let ((v (last p))
        (w (first q)))
    (if (and (far-out v)
             (far-out w)
             (>= (abs (- v w)) path-outer-delta))
        ;; Two far-apart far-out points.  Try to walk around
        ;;  outside the perimeter, in the shorter direction.
        (let* ((pdiff (angle (/ v w)))
               (npoints (floor (abs pdiff) (asin .2)))
               (delta (/ pdiff (+ npoints 1)))
               (incr (cis delta)))
          (do ((j 0 (+ j 1))
               (p (list w "end splice") (cons (* (car p) incr) p)))
              ((= j npoints) (cons "start splice" p))))
        '())))

;;; This function draws the annuli for the pattern.
(defun shaded-annulus (inner outer sectors firstshade lastshade fn stream)
  (check-arg (lambda (s) (zero? (modulo s 4))) sectors)
  (comment-line stream "Annulus ~S ~S ~S ~S ~S"
                (round-real inner) (round-real outer)
                sectors firstshade lastshade)
  (dotimes (jj sectors)
    (let ((j (- sectors jj 1)))
      (let* ((lophase (+ tiny (* 2 pi (/ j sectors))))
             (hiphase (* 2 pi (/ (+ j 1) sectors)))
             (midphase (/ (+ lophase hiphase) 2.0))
             (midradius (/ (+ inner outer) 2.0))
             (quadrant (floor (/ (* j 4) sectors))))
        (comment-line stream "Sector from ~S to ~S (quadrant ~S)"
                      (round-real lophase)
                      (round-real hiphase)
                      quadrant)
        (let ((p0 (reverse (parametric-path midradius
                                            inner
                                            (radial lophase quadrant)
                                            fn)))
              (p1 (parametric-path midradius
                                   outer
                                   (radial lophase quadrant)
                                   fn))
              (p2 (reverse (parametric-path midphase
                                            lophase
                                            (circumferential outer
                                                             quadrant)
                                            fn)))
              (p3 (parametric-path midphase
                                   hiphase
                                   (circumferential outer quadrant)
                                   fn))
              (p4 (reverse (parametric-path midradius
                                            outer
                                            (radial hiphase quadrant)
                                            fn)))
              (p5 (parametric-path midradius
                                   inner
                                   (radial hiphase quadrant)
                                   fn))
              (p6 (reverse (parametric-path midphase
                                            hiphase
                                            (circumferential inner
                                                             quadrant)
                                            fn)))
              (p7 (parametric-path midphase
                                   lophase
                                   (circumferential inner quadrant)
                                   fn)))
          (postscript-closed-path stream
            (append
              p0 (splice p0 p1) '("middle radial")
              p1 (splice p1 p2) '("end radial")
              p2 (splice p2 p3) '("middle circumferential")
              p3 (splice p3 p4) '("end circumferential")
              p4 (splice p4 p5) '("middle radial")
              p5 (splice p5 p6) '("end radial")
              p6 (splice p6 p7) '("middle circumferential")
              p7 (splice p7 p0) '("end circumferential")
              )))
        (postscript-shade stream
                          (/ (+ (* firstshade (- (- sectors 1) j))
                                (* lastshade j))
                             (- sectors 1)))))))

(defun postscript-penstroke (stream wid)
  (format stream "\n~S setlinewidth   1 setlinecap  stroke"
          wid))

(defun postscript-shade (stream shade)
  (format stream "\ncurrentgray   ~S setgray   fill   setgray"
          shade))

(defun postscript-closed-path (stream path)
  (unless (every far-out (filter number? path))
    (postscript-raw-path stream path)
    (format stream "\n  closepath")))

(defun postscript-path (stream path)
  (unless (every far-out (filter number? path))
    (postscript-raw-path stream path)))

;;; Print a path as a series of PostScript "lineto" commands.
(defun postscript-raw-path (stream path)
  (format stream "\nnewpath")
  (let ((fmt "\n  ~S ~S moveto"))
    (dolist (pt path)
      (cond ((string? pt)
             (format stream "\n  %~A" pt))
            (else (format stream
                       fmt
                       (clamp-real (real-part pt))
                       (clamp-real (imag-part pt)))
                  (set! fmt "\n  ~S ~S lineto"))))))

;;; Definitions of functions to be plotted that are not
;;; standard Common Lisp functions.

(defun one-plus-over-one-minus (x) (/ (+ 1 x) (- 1 x)))

(defun one-minus-over-one-plus (x) (/ (- 1 x) (+ 1 x)))

(defun sqrt-square-minus-one (x) (sqrt (- 1 (* x x))))

(defun sqrt-one-plus-square (x) (sqrt (+ 1 (* x x))))

;;; Because X3J13 voted for a new definition of the atan function,
;;; the following definition was used in place of the atan function
;;; provided by the Common Lisp implementation I was using.

(defun good-atan (x)
  (/ (- (log (+ 1 (* x +i)))
        (log (- 1 (* x +i))))
     +2i))

;;; Because the first edition had an erroneous definition of atanh,
;;; the following definition was used in place of the atanh function
;;; provided by the Common Lisp implementation I was using.

(defun really-good-atanh (x)
  (/ (- (log (+ 1 x))
        (log (- 1 x)))
     2))

;;; This is the main procedure that is intended to be called by a user.
(defun picture (fn name)
  (call-with-output-file (string-append (string-downcase name)
                                        "-plot.ps")
    (lambda (stream)
      (format stream "% PostScript file for plot of function ~S\n" fn)
      (format stream "% Plot is to fit in a region ~S inches square\n"
              (/ text-width-in-picas 6.0))
      (format stream
              "%  showing axes extending ~S units from the origin.\n"
              units-to-show)
      (let ((scaling (/ (* text-width-in-picas 12) (* units-to-show 2))))
        (format stream "\n~S ~S scale" scaling scaling))
      (format stream "\n~S ~S translate" units-to-show units-to-show)
      (format stream "\nnewpath")
      (format stream "\n  ~S ~S moveto" (- units-to-show) (- units-to-show))
      (format stream "\n  ~S ~S lineto" units-to-show (- units-to-show))
      (format stream "\n  ~S ~S lineto" units-to-show units-to-show)
      (format stream "\n  ~S ~S lineto" (- units-to-show) units-to-show)
      (format stream "\n  closepath")
      (format stream "\nclip")
      (moby-grid fn stream)
      (format stream
              "\n% End of PostScript file for plot of function ~S"
              fn)
      (newline stream))))

