;;
;; This is an excerpt of the example in the section
;; "Introduction to the object system" in the reference manual.
;;

(use srfi-1)

;;;
;;; 2D points
;;;
(define-class <2d-point> ()
  ((x :init-value 0.0 :init-keyword :x :accessor x-of)
   (y :init-value 0.0 :init-keyword :y :accessor y-of)))

(define-class <2d-vector> ()
  ((x :init-value 0.0 :init-keyword :x :accessor x-of)
   (y :init-value 0.0 :init-keyword :y :accessor y-of)))

;; Define some methods
(define-method move-by! ((pt <2d-point>) dx dy)
  (inc! (x-of pt) dx)
  (inc! (y-of pt) dy))

(define-method move-by! ((pt <2d-point>) (vec <2d-vector>))
  (move-by! pt (x-of vec) (y-of vec)))

(define-method move-by! ((pt <2d-point>) (c <complex>))
  (move-by! pt (real-part c) (imag-part c)))

(define-method point->list ((pt <2d-point>))
  (list (x-of pt) (y-of pt)))

;; Define a customized writer and comparison methods
(define-method write-object ((pt <2d-point>) port)
  (format port "[[~a, ~a]]" (x-of pt) (y-of pt)))

(define-method write-object ((vec <2d-vector>) port)
  (format port "<<~a, ~a>>" (x-of vec) (y-of vec)))

(define-method object-equal? ((a <2d-point>) (b <2d-point>))
  (and (equal? (x-of a) (x-of b))
       (equal? (y-of a) (y-of b))))

;;;
;;; Drawing shapes
;;;
(define-class <shape> ()
  ((color     :init-value '(0 0 0) :init-keyword :color)
   (thickness :init-value 2 :init-keyword :thickness)))

(define *shapes* '())  ;; global shape list

(define-method initialize ((self <shape>) initargs)
  (next-method)
  (push! *shapes* self))

(define-class <ps-device> () ())

(define-class <point-shape> (<shape>)
  ((point  :init-form (make <2d-point>) :init-keyword :point)))

(define-class <polyline-shape> (<shape>)
  ((points :init-value '() :init-keyword :points)
   (closed :init-value #f  :init-keyword :closed)))

(define-method draw ((shapes <list>) (device <ps-device>))
  (format #t "%%\n")
  (for-each (cut draw <> device) shapes)
  (format #t "showpage\n"))

(define-method draw ((self <shape>) (device <ps-device>))
  (format #t "gsave\n")
  (draw-path self device)
  (apply format #t "~a ~a ~a setrgbcolor\n" (ref self 'color))
  (format #t "~a setlinewidth\n" (ref self 'thickness))
  (format #t "stroke\n")
  (format #t "grestore\n"))

(define-method draw-path ((self <point-shape>) (device <ps-device>))
  (apply format #t "newpath ~a ~a 1 0 360 arc closepath\n"
         (point->list (ref self 'point))))

(define-method draw-path ((self <polyline-shape>) (device <ps-device>))
  (let ((pts (ref self 'points)))
    (when (>= (length pts) 2)
      (format #t "newpath\n")
      (apply format #t "~a ~a moveto\n" (point->list (car pts)))
      (for-each (lambda (pt)
                  (apply format #t "~a ~a lineto\n" (point->list pt)))
                (cdr pts))
      (when (ref self 'closed)
        (apply format #t "~a ~a lineto\n" (point->list (car pts))))
      (format #t "closepath\n"))))

;;;
;;; A sample shape
;;;
(use math.const)

(define (shape-sample)

  ;; creates 5 corner points of pentagon
  (define (make-corners scale)
    (map (lambda (i)
           (let ((pt (make <2d-point>)))
             (move-by! pt (make-polar scale (* i 2/5 pi)))
             (move-by! pt 200 200)
             pt))
         (iota 5)))

  (set! *shapes* '())  ;; clear the shape list
  (let* ((corners (make-corners 100)))
    ;; a pentagon in green
    (make <polyline-shape>
      :color '(0 1 0) :closed #t
      :points corners)
    ;; a star-shape in blue
    (make <polyline-shape>
      :color '(1 0 0) :closed #t
      :points (list (list-ref corners 0)
                    (list-ref corners 2)
                    (list-ref corners 4)
                    (list-ref corners 1)
                    (list-ref corners 3)))
    ;; put dots in each corner of the star
    (for-each (cut make <point-shape> :point <>)
              (make-corners 90))
    ;; draw the shapes
    (draw *shapes* (make <ps-device>)))
  )
  
;; (with-output-to-file "oointro.ps" shape-sample)
