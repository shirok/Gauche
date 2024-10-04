;;
;; test for sha1 module
;;

(use gauche.test)
(test-start "sha1")
(test-section "sha1")
(use srfi.13)
(use srfi.42)
(use srfi.152)
(use gauche.uvector)
(use scheme.bitwise)
(use file.util)
(use util.match)

(use rfc.sha1)
(test-module 'rfc.sha1)

(for-each
 (^[args]
   (test* "sha1-digest-string" (car args)
          (digest-hexify (apply sha1-digest-string (cdr args))))
   (test* "digest-string" (car args)
          (digest-hexify (apply digest-string <sha1> (cdr args)))))
 `(("a9993e364706816aba3e25717850c26c9cd0d89d" "abc")
   ("84983e441c3bd26ebaae4aa1f95129e5e54670f1" "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
   ("34aa973cd4c4daa4f61eeb2bdbad27316534016f"
    ,(make-string 1000000 #\a))
   ("dea356a2cddd90c7a7ecedc5ebb563934f460452"
    ,(with-output-to-string (^[] (dotimes (n 10) (display "0123456701234567012345670123456701234567012345670123456701234567")))))))

(test-section "sha2")

(use rfc.sha)
(test-module 'rfc.sha)

(define (parse-infofile file)
  (define tab (make-hash-table 'eq?))
  (define source #f)
  (define repeat 1)
  (define (loop ls)
    (match ls
      [() tab]
      [("SHA1:" . ls) (gather 'sha1 ls '())]
      [("SHA224:" . ls) (gather 'sha224 ls '())]
      [("SHA256:" . ls) (gather 'sha256 ls '())]
      [("SHA384:" . ls) (gather 'sha384 ls '())]
      [("SHA512:" . ls) (gather 'sha512 ls '())]
      [("ASCII:" . ls)  (get-ascii ls '())]
      [("HEX:" . ls)    (get-hex ls '())]
      [("REPEAT:" cnt . ls) (set! repeat (x->integer cnt)) (loop ls)]
      [("RANGE:" rng . ls)
       (rxmatch-let (#/(\d+)-(\d+)/ rng) (_ s e)
         (let ([s (x->integer s)]
               [e (x->integer e)])
           (set! source (string-ec (: i s (+ e 1)) (integer->char i)))))
       (loop ls)]
      [(_ . ls) (loop ls)]))
  (define (gather tag ls r)
    (match ls
      [(or ("" . ls) (and () ls))
       (set! (ref tab tag) (string-concatenate-reverse r)) (loop ls)]
      [(x . ls) (gather tag ls (cons x r))]))
  (define (get-ascii ls r)
    (match ls
      [(or ("" . ls) (and () ls))
       (set! source (string-concatenate-reverse r)) (loop ls)]
      [(x . ls) (get-ascii ls (cons (string-trim-both x #[\"]) r))]))
  (define (get-hex ls r)
    (match ls
      [(or ("" . ls) (and () ls))
       (set! source (unhexify (string-concatenate-reverse r))) (loop ls)]
      [(x . ls) (=> fail)
       (if (string-prefix? "0x" x)
         (get-hex ls (cons (string-drop x 2) r))
         (fail))]))
  (define (unhexify s)
    (with-output-to-string
      (^[] (let loop ([s s])
             (unless (string-null? s)
               (write-byte (string->number (string-take s 2) 16))
               (loop (string-drop s 2)))))))

  (let1 tab (loop (map string-trim-both (file->string-list file)))
    (unless source (error "no source found in:" file))
    (cons (if (> repeat 1)
            (string-concatenate (make-list repeat source))
            source)
          tab)))

(define (test-from-file file)
  (match-let1 (input . results) (parse-infofile file)
    (if-let1 r (ref results 'sha1 #f)
      (test* #"~file sha1" r (digest-message-to 'hex <sha1> input)))
    (if-let1 r (ref results 'sha224 #f)
      (test* #"~file sha224" r (digest-message-to 'hex <sha224> input)))
    (if-let1 r (ref results 'sha256 #f)
      (test* #"~file sha256" r (digest-message-to 'hex <sha256> input)))
    (if-let1 r (ref results 'sha384 #f)
      (test* #"~file sha384" r (digest-message-to 'hex <sha384> input)))
    (if-let1 r (ref results 'sha512 #f)
      (test* #"~file sha512" r (digest-message-to 'hex <sha512> input)))
    ))

(for-each test-from-file (glob "data/*.info"))

(test-section "sha3")

;; SHA3 tests are taken from
;; https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines/example-values

(define *sha3-tests*
  ;; (input sha3-224 sha3-256 sha3-384 sha3-512)
  '(;; Empty input
    (""
     "6B 4E 03 42 36 67 DB B7 3B 6E 15 45 4F 0E B1 AB\
     D4 59 7F 9A 1B 07 8E 3F 5B 5A 6B C7 "
     "A7 FF C6 F8 BF 1E D7 66 51 C1 47 56 A0 61 D6 62\
     F5 80 FF 4D E4 3B 49 FA 82 D8 0A 4B 80 F8 43 4A"
     "0C 63 A7 5B 84 5E 4F 7D 01 10 7D 85 2E 4C 24 85\
     C5 1A 50 AA AA 94 FC 61 99 5E 71 BB EE 98 3A 2A\
     C3 71 38 31 26 4A DB 47 FB 6B D1 E0 58 D5 F0 04"
     "A6 9F 73 CC A2 3A 9A C5 C8 B5 67 DC 18 5A 75 6E\
     97 C9 82 16 4F E2 58 59 E0 D1 DC C1 47 5C 80 A6\
     15 B2 12 3A F1 F5 F9 4C 11 E3 E9 40 2C 3A C5 58\
     F5 00 19 9D 95 B6 D3 E3 01 75 85 86 28 1D CD 26")
    ;; 1600-bit input
    ("1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 \
      1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1 1 1 0 0 0 1 0 1"
     "93 76 81 6A BA 50 3F 72 F9 6C E7 EB 65 AC 09 5D\
     EE E3 BE 4B F9 BB C2 A1 CB 7E 11 E0"
     "79 F3 8A DE C5 C2 03 07 A9 8E F7 6E 83 24 AF BF\
     D4 6C FD 81 B2 2E 39 73 C6 5F A1 BD 9D E3 17 87"
     "18 81 DE 2C A7 E4 1E F9 5D C4 73 2B 8F 5F 00 2B\
     18 9C C1 E4 2B 74 16 8E D1 73 26 49 CE 1D BC DD\
     76 19 7A 31 FD 55 EE 98 9F 2D 70 50 DD 47 3E 8F"
     "E7 6D FA D2 20 84 A8 B1 46 7F CF 2F FA 58 36 1B\
     EC 76 28 ED F5 F3 FD C0 E4 80 5D C4 8C AE EC A8\
     1B 7C 13 C3 0A DF 52 A3 65 95 84 73 9A 2D F4 6B\
     E5 89 C5 1C A1 A4 A8 41 6D F6 54 5A 1C E8 BA 00")
    ))

(define (sha3-test class input expect)
  (let ([binput ($ list->u8vector
                   $ map (^[lis]
                           (string->number
                            (string-filter #[\d] (write-to-string (reverse lis)))
                            2))
                   $ slices (read-from-string (string-append "(" input ")")) 8)]
        [bexpect (string-remove #[\s] expect)])
    (test* (format "~a ~a" class (string-count input #[\d])) bexpect
           (digest-message-to 'base16 class binput))))

(dolist [samples *sha3-tests*]
  (match-let1 (input sha3-224-expect sha3-256-expect sha3-384-expect sha3-512-expect)
      samples
    (sha3-test <sha3-224> input sha3-224-expect)
    (sha3-test <sha3-256> input sha3-256-expect)
    (sha3-test <sha3-384> input sha3-384-expect)
    (sha3-test <sha3-512> input sha3-512-expect)
    ))

(test-end)
