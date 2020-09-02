(define (check-quasi-integer-operations)
  (print-header "Checking quasi-integer operations...")

  (check (bitvector= (bitvector-logical-shift (bitvector 1 0 1 1) 2 0)
                     (bitvector 1 1 0 0))
   => #t)
  (check (bitvector= (bitvector-logical-shift (bitvector 1 0 1 1) -2 #t)
                     (bitvector 1 1 1 0))
   => #t)

  (check (bitvector-count 1 (make-bitvector 8 1))        => 8)
  (check (bitvector-count #t (make-bitvector 8 0))       => 0)
  (check (bitvector-count 1 (bitvector 1 1 0 1 1 0 0 0)) => 4)

  (check (bitvector-count-run 1 (make-bitvector 8 1) 0)  => 8)
  (check (bitvector-count-run #t (make-bitvector 8 0) 4) => 0)
  (check (bitvector-count-run 1 (bitvector 0 1 1 1) 1)   => 3)

  (let ((then-bvec (bitvector 1 0 1 0))
        (else-bvec (bitvector 0 0 0 1)))
    (check
     (bitvector= (bitvector-if (make-bitvector 4 1) then-bvec else-bvec)
                 then-bvec)
     => #t)
    (check
     (bitvector= (bitvector-if (make-bitvector 4 0) then-bvec else-bvec)
                 else-bvec)
     => #t))
  (check (bitvector= (bitvector-if (bitvector 1 1 0 0)
                                   (bitvector 0 1 1 1)
                                   (bitvector 0 0 1 0))
                     (bitvector 0 1 1 0))
   => #t)

  (check (bitvector-first-bit 0 (make-bitvector 4 0))  => 0)
  (check (bitvector-first-bit #t (bitvector 0 0 1 0))  => 2)
  (check (bitvector-first-bit #f (make-bitvector 4 1)) => -1)
)
