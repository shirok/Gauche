;; Knuth-Morris-Pratt search constructs.
;; I included this just for completeness.
;; The SRFI-13 specification assumes accessing the pattern by index is
;; a lightweight operation, but it may not be in Gauche.  
;; So the programs using these functions may not be very efficient, in
;; spite of the efforts for efficiency put in the original SRFI design.
;;
;; The right way in Gauche is passing string-pointer pointing to the pattern
;; instead of carrying around the three arguments (pattern, p-start and i).

(define (make-kmp-restart-vector s . args)
  (let-optional* args ((c= char=?) start end)
    (let* ((pattern (%maybe-substring s start end))
           (rv (make-vector (string-length pattern) -1)))
      (


