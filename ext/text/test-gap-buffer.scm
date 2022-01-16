;;-------------------------------------------------------------------
(test-section "gap-buffer")
(use text.gap-buffer)
(use gauche.uvector)

(test-module 'text.gap-buffer)

;; stringify gap-buffer for test purpose
(define (gap-buffer-visualize gbuf)
  (with-output-to-string
    (^[]
      (let1 buf (~ gbuf'buffer) ;accessing internal
        (dotimes [i (~ gbuf'gap-start)]
          (display (integer->char (~ buf i))))
        (dotimes [i (- (~ gbuf'gap-end) (~ gbuf'gap-start))]
          (display #\_))
        (dotimes [i (- (gap-buffer-capacity gbuf) (~ gbuf'gap-end))]
          (display (integer->char (~ buf (+ i (~ gbuf'gap-end))))))))))

(test* "constuct" "abcde___"
       (gap-buffer-visualize (string->gap-buffer "abcde")))
(test* "constuct" "___abcde"
       (gap-buffer-visualize (string->gap-buffer "abcde" 0 'beginning)))
(test* "constuct" "bcd_"
       (gap-buffer-visualize (string->gap-buffer "abcde" 0 'end 1 4)))

(let1 gbuf (string->gap-buffer "abcde")
  (test* "move" "abcd___e"
         (gap-buffer-visualize (gap-buffer-move! gbuf -1 'current)))
  (test* "move" "a___bcde"
         (gap-buffer-visualize (gap-buffer-move! gbuf 1)))
  (test* "move" "abc___de"
         (gap-buffer-visualize (gap-buffer-move! gbuf -2 'end)))
  (test* "insert" "abcZ__de"
         (gap-buffer-visualize (gap-buffer-insert! gbuf #\Z)))
  (test* "insert" "abcZxyde"
         (gap-buffer-visualize (gap-buffer-insert! gbuf "xy")))
  (test* "insert" "abcZxyw_______de"
         (gap-buffer-visualize (gap-buffer-insert! gbuf #\w)))
  (test* "insert"
         "abcZxyw012345678901234567890123456789_________________________de"
         (gap-buffer-visualize
          (gap-buffer-insert! gbuf "012345678901234567890123456789")))
  (test* "delete"
         "abcZxyw012345678901234567890123456789__________________________e"
         (gap-buffer-visualize
          (gap-buffer-delete! gbuf 1)))
  (test* "delete"
         "abc____________________________________678901234567890123456789e"
         (begin (gap-buffer-move! gbuf 3)
                (gap-buffer-visualize
                 (gap-buffer-delete! gbuf 10))))
  (test* "delete"
         (test-error)
         (gap-buffer-visualize (gap-buffer-delete! gbuf 100)))
  (test* "replace"
         "abcdef______________________________________1234567890123456789e"
         (gap-buffer-visualize (gap-buffer-replace! gbuf 5 "def")))
  )

(let1 gbuf (string->gap-buffer "abcde")
  (test* "->string (gap at end)" "abcde"
         (gap-buffer->string gbuf))
  (test* "->string (gap at beginning)" "abcde"
         (begin (gap-buffer-move! gbuf 0)
                (gap-buffer->string gbuf)))
  (test* "->string (gap at middle)" "abcde"
         (begin (gap-buffer-move! gbuf 3 'current)
                (gap-buffer->string gbuf)))
  )

(let1 gbuf (make-gap-buffer :initial-capacity 32)
  (gap-buffer-insert! gbuf "abcde")
  (gap-buffer-move! gbuf 2)
  (test* "gap-buffer-pos-at-end?" '(#f #f #t)
         (list (gap-buffer-pos-at-end? gbuf 0)
               (gap-buffer-pos-at-end? gbuf 2)
               (gap-buffer-pos-at-end? gbuf 5)))
  )

(let1 gbuf (gap-buffer-copy (string->gap-buffer "abcde"))
  (test* "undo and redo"
         '("abXYZcde" "abXYde" "aQRSYde" "abXYde" "abXYZcde"
           "abcde" "aQRSYde" "abcde")
         (let* ([u0 (begin (gap-buffer-move! gbuf 2)
                           (gap-buffer-edit! gbuf '(i #f "XYZ")))]
                [s0 (gap-buffer->string gbuf)] ; abXYZcde
                [u1 (begin (gap-buffer-move! gbuf -1 'current)
                           (gap-buffer-edit! gbuf '(d #f 2)))]
                [s1 (gap-buffer->string gbuf)] ; abXYde
                [u2 (begin (gap-buffer-move! gbuf -3 'current)
                           (gap-buffer-edit! gbuf '(c #f 2 "QRS")))]
                [s2 (gap-buffer->string gbuf)] ; aQRSYde
                [r2 (begin (gap-buffer-move! gbuf 0)
                           (gap-buffer-edit! gbuf u2))] ; undo change!
                [s3 (gap-buffer->string gbuf)] ; abXYde
                [r1 (gap-buffer-edit! gbuf u1)] ; undo delete!
                [s4 (gap-buffer->string gbuf)] ; abXYZcde
                [r0 (begin (gap-buffer-move! gbuf -1 'end)
                           (gap-buffer-edit! gbuf u0))] ; undo insert!
                [s5 (gap-buffer->string gbuf)] ; abcde
                [u0 (gap-buffer-edit! gbuf r0)] ; redo insert!
                [u1 (gap-buffer-edit! gbuf r1)] ; redo delete!
                [u2 (gap-buffer-edit! gbuf r2)] ; redo change!
                [s6 (gap-buffer->string gbuf)] ; aQRSYde
                [s7 (begin (gap-buffer-edit! gbuf u2)
                           (gap-buffer-edit! gbuf u1)
                           (gap-buffer-edit! gbuf u0)
                           (gap-buffer->string gbuf))] ; abcde
                )
           (list s0 s1 s2 s3 s4 s5 s6 s7)))
  )
