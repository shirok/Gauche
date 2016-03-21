;;
;; snake game - sample for raw console manipulation
;;

(use gauche.termios)
(use gauche.parameter)
(use gauche.array)
(use util.match)
(use srfi-27)
(use text.console)

(define (main args)
  (call-with-console (guard (e [else (exit 1 (~ e'message))])
                       (make-default-console))
                     game)
  0)

;; snake : (<dir> <head> <tail> ...)
;; <head> : <pt>
;; <tail> : <pt>
;; <pt> : (<x> . <y>)
;; <dir> : N | E | W | S
(define (snake-dir snake) (car snake))
(define (snake-body snake) (cdr snake))
(define (snake-head snake) (car (snake-body snake)))
(define (snake-tail snake) (cdr (snake-body snake)))

;; NB: For the time being, we avoid writing the wall to the last row;
;; Writing on the bottom-right corner causes the screen to scroll in
;; <windows-console>.  We can avoid it by SetConsoleMode but currently we're
;; not sure how to handle it in the console abstraction layer (we don't want
;; the wrap-to-next-line behavior off by default).
(define (game con)
  (hide-cursor con)
  (clear-screen con)
  (random-source-randomize! default-random-source)
  (receive (row col) (query-screen-size con)
    (let* ([field (new-field (- row 1) col)]
           [snake (new-snake field)])
      (render-field con field)
      (run-game con field snake #f (new-food field snake) 0))))

(define (collide-wall? field x y) (array-ref field y x))

(define (collide-snake? snake x y)
  (any (^p (and (eqv? (car p) x) (eqv? (cdr p) y))) (cdr snake)))

(define (new-field row col)
  (rlet1 field (make-array (shape 0 row 0 col) #f)
    (dotimes [k col]
      (array-set! field 0 k #t)
      (array-set! field (- row 1) k #t))
    (dotimes [k row]
      (array-set! field k 0 #t)
      (array-set! field k (- col 1) #t))
    (dotimes [k (* (quotient row 5) 2)]
      (array-set! field k (ash col -1) #t)
      (array-set! field (- row k 1) (ash col -1) #t))))

(define (new-snake field)
  (let ([row (array-end field 0)]
        [col (array-end field 1)])
    (let* ([t (cons (random-integer col) (random-integer row))]
           [dir (~ '(N E W S) (random-integer 4))]
           [h (next-point t dir)])
      (if (or (collide-wall? field (car t) (cdr t))
              (collide-wall? field (car h) (cdr h)))
        (new-snake field)
        `(,dir ,h ,t)))))

(define (update-snake snake dir new-head extend?)
  `(,dir ,new-head
         ,@(if extend? (snake-body snake) (drop-right (snake-body snake) 1))))

(define (new-food field snake) ; returns food location (x . y)
  (let ([row (array-end field 0)]
        [col (array-end field 1)])
    (let ([x (random-integer col)]
          [y (random-integer row)])
      (if (or (collide-wall? field x y)
              (collide-snake? snake x y))
        (new-food field snake)
        (cons x y)))))

(define (next-point point dir)
  (match-let1 (x . y) point
    (cons (case dir [(E) (+ x 1)] [(W) (- x 1)] [else x])
          (case dir [(S) (+ y 1)] [(N) (- y 1)] [else y]))))

(define (find-food? snake dir food)
  (equal? food (next-point (snake-head snake) dir)))

(define (run-game con field snake prev-snake food score)
  (render con field snake prev-snake food score)
  ;; trick - since height of character is larger than width, if we use the
  ;; same interval, it would look like the snake runs faster in N-S direction
  ;; than E-W direction.
  (case (snake-dir snake)
    [(N S) (sys-nanosleep #e18e7)]
    [(E W) (sys-nanosleep #e9e7)])
  (let1 dir (or (and-let* ([newdir (get-dir con)])
                  (case (snake-dir snake) ; do now allow to turn 180 deg.
                    [(N) (and (not (eq? newdir 'S)) newdir)]
                    [(E) (and (not (eq? newdir 'W)) newdir)]
                    [(S) (and (not (eq? newdir 'N)) newdir)]
                    [(W) (and (not (eq? newdir 'E)) newdir)]))
                (snake-dir snake))
    (match-let1 (and (x . y) hd) (next-point (snake-head snake) dir)
      (cond [(or (collide-wall? field x y)
                 (collide-snake? snake x y))
             (render con field (update-snake snake dir hd #f) snake food score)
             (game-over con field)]
            [(find-food? snake dir food)
             (let1 snake. (update-snake snake dir hd #t)
               (run-game con field snake. snake 
                         (new-food field snake.) (+ score 1)))]
            [else
             (run-game con field (update-snake snake dir hd #f) snake
                       food score)]))))

(define (get-dir con) ;returns W, S, N, E or #f
  (and (chready? con)
       (case (getch con)
         [(#\h KEY_LEFT)  'W]
         [(#\j KEY_DOWN)  'S]
         [(#\k KEY_UP)    'N]
         [(#\l KEY_RIGHT) 'E]
         [else #f])))

(define (render con field snake prev-snake food score)
  (set-character-attribute con '(green black bright))
  (render-snake con snake prev-snake)
  (set-character-attribute con '(magenta black bright))
  (render-point con food #\o)
  (set-character-attribute con '(white black))
  (move-cursor-to con
                  (array-length field 0) ;the bottom row
                  5)
  (putstr con (format "S C O R E : ~3d" score)))

(define (render-field con field)
  (set-character-attribute con '(black black))
  (clear-screen con)
  (set-character-attribute con '(cyan blue))
  ($ array-for-each-index field
     (^[y x]
       (when (array-ref field y x)
         (move-cursor-to con y x)
         (putch con #\#)))))

(define (render-snake con snake prev-snake)
  ;; We only need to draw difference of snake and prev-snake.
  ;; NB: prev-snake can be #f for the initial state.
  (render-point con (snake-head snake) #\@)
  (if prev-snake
    (begin
      (when (= (length (snake-body snake)) (length (snake-body prev-snake)))
        (render-point con (last (snake-tail prev-snake)) #\space))
      (render-point con (snake-head prev-snake) #\*))
    (dolist [pt (snake-tail snake)]
      (render-point con pt #\*))))

(define (render-point con pt ch)
  (match-let1 (x . y) pt
    (move-cursor-to con y x)
    (putch con ch)))

(define (game-over con field)
  (let ([height (array-length field 0)]
        [width  (array-length field 1)])
    (move-cursor-to con (ash height -1) (- (ash width -1) 5))
    (set-character-attribute con '(white black reverse))
    (putstr con "Game over!")
    ;; Exit game with any keypress; however, we don't want to pick
    ;; the keypress right after game over, so wait for a sec, discard
    ;; whatever keys pressed during that, then wait for input.
    (sys-sleep 1)
    (while (chready? con) (getch con))
    (getch con)
    (reset-terminal con)))
