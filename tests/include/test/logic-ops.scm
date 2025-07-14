(define (check-bitwise-operations)
  (define test-bvec1 (bitvector 1 0 1 0))
  (define test-bvec2 (bitvector 1 1 0 0))
  (define test-bvec3 (bitvector 0 0 1 1))
  (print-header "Checking bitwise operations...")

  ;;; not

  (check (bitvector= (bitvector-not test-bvec1) (bitvector 0 1 0 1))
   => #t)
  (check (bitvector= (bitvector-not (bitvector-not test-bvec1))
                     test-bvec1)
   => #t)

  ;;; Associative operations

  (check (bitvector= (bitvector-and test-bvec1 test-bvec2 test-bvec3)
                     (bitvector 0 0 0 0))
   => #t)
  (check (bitvector= (bitvector-ior test-bvec1 test-bvec2 test-bvec3)
                     (bitvector 1 1 1 1))
   => #t)
  (check (bitvector= (bitvector-xor test-bvec1 test-bvec2 test-bvec3)
                     (bitvector 0 1 0 1))
   => #t)
  (check (bitvector= (bitvector-eqv test-bvec1 test-bvec2 test-bvec3)
                     (bitvector 0 1 0 1))
   => #t)

  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-and! test-bvec1* test-bvec2 test-bvec3)
                  test-bvec1*)
                 (bitvector 0 0 0 0))
     => #t))
  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-ior! test-bvec1* test-bvec2 test-bvec3)
                  test-bvec1*)
                 (bitvector 1 1 1 1))
     => #t))
  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-xor! test-bvec1* test-bvec2 test-bvec3)
                  test-bvec1*)
                 (bitvector 0 1 0 1))
     => #t))
  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-eqv! test-bvec1* test-bvec2 test-bvec3)
                  test-bvec1*)
                 (bitvector 0 1 0 1))
     => #t))

  ;;; Non-associative binary operations

  (check (bitvector= (bitvector-nand test-bvec1 test-bvec2)
                     (bitvector 0 1 1 1))
   => #t)
  (check (bitvector= (bitvector-nor test-bvec1 test-bvec2)
                     (bitvector 0 0 0 1))
   => #t)
  (check (bitvector= (bitvector-andc1 test-bvec1 test-bvec2)
                     (bitvector 0 1 0 0))
   => #t)
  (check (bitvector= (bitvector-andc2 test-bvec1 test-bvec2)
                     (bitvector 0 0 1 0))
   => #t)
  (check (bitvector= (bitvector-orc1 test-bvec1 test-bvec2)
                     (bitvector 1 1 0 1))
   => #t)
  (check (bitvector= (bitvector-orc2 test-bvec1 test-bvec2)
                     (bitvector 1 0 1 1))
   => #t)

  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-nand! test-bvec1* test-bvec2)
                  test-bvec1*)
                 (bitvector 0 1 1 1))
     => #t))
  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-nor! test-bvec1* test-bvec2)
                  test-bvec1*)
                 (bitvector 0 0 0 1))
     => #t))
  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-andc1! test-bvec1* test-bvec2)
                  test-bvec1*)
                 (bitvector 0 1 0 0))
     => #t))
  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-andc2! test-bvec1* test-bvec2)
                  test-bvec1*)
                 (bitvector 0 0 1 0))
     => #t))
  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-orc1! test-bvec1* test-bvec2)
                  test-bvec1*)
                 (bitvector 1 1 0 1))
     => #t))
  (let ((test-bvec1* (bitvector-copy test-bvec1)))
    (check
     (bitvector= (begin
                  (bitvector-orc2! test-bvec1* test-bvec2)
                  test-bvec1*)
                 (bitvector 1 0 1 1))
     => #t))
)
