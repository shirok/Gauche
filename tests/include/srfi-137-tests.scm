;;(define-library (cowan types test)
;;  (export run-tests)
;;  (import (scheme base)
;;          (srfi 64)
;;          (cowan types))
;;  (begin
    (define (run-tests)
      (test-begin "Unique run-time types")

      (test-equal "Type payload"
                  'reia
                  (let-values
                      (((reia-payload
                         make-reia reia?
                         reia-ref
                         make-reia-subtype)
                        (make-type 'reia)))
                    (reia-payload)))

      (test-assert "Disjoint procedures"
                   (let-values
                       (((reia-payload1 . reia1*)
                         (make-type 'reia))
                        ((reia-payload2 . reia2*)
                         (make-type 'reia)))
                     (not (eq? reia-payload1 reia-payload2))))

      (test-begin "Type predicates and subtypes")

      (let*-values
          (((reia-payload
             make-reia
             reia?
             reia-ref
             make-reia-subtype)
            (make-type 'reia))
           ((daughter-payload
             make-daughter
             daughter?
             daughter-ref
             make-daughter-subtype)
            (make-reia-subtype 'daughter))
           ((son-payload
             make-son
             son?
             son-ref
             make-son-subtype)
            (make-reia-subtype 'son))
           ((grand-daughter-payload
             make-grand-daughter
             grand-daughter?
             grand-daughter-ref
             make-grand-daughter-subtype)
            (make-daughter-subtype 'grand-daughter)))
        (test-assert "Instance fulfills predicate"
                     (reia? (make-reia #f)))
        (test-assert "Instance of subtype fulfills predicate"
                     (reia? (make-daughter #f)))
        (test-assert "Instance of supertype does not fulfill predicate"
                     (not (daughter? (make-reia #f))))
        (test-assert "Instance of peertype does not fulfill predicate"
                     (not (son? (make-daughter #f))))
        (test-assert "Instance of indirect subtype fulfills predicate"
                     (reia? (make-grand-daughter #f))))

      (test-end)

      (test-equal "Instance payload"
                  'payload
                  (let-values
                      (((reia-payload
                         make-reia
                         reia?
                         reia-ref
                         make-reia-subtype)
                        (make-type 'reia)))
                    (reia-ref (make-reia 'payload))))

      (test-end))
;;))
