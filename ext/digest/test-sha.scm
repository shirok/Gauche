;;
;; test for sha1 module
;;

(test-section "sha1")
(use srfi-13)
(use srfi-42)
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
      (test* #"~file sha1" r (digest-hexify (sha1-digest-string input))))
    (if-let1 r (ref results 'sha224 #f)
      (test* #"~file sha224" r (digest-hexify (sha224-digest-string input))))
    (if-let1 r (ref results 'sha256 #f)
      (test* #"~file sha256" r (digest-hexify (sha256-digest-string input))))
    (if-let1 r (ref results 'sha384 #f)
      (test* #"~file sha384" r (digest-hexify (sha384-digest-string input))))
    (if-let1 r (ref results 'sha512 #f)
      (test* #"~file sha512" r (digest-hexify (sha512-digest-string input))))
    ))

(for-each test-from-file (glob "data/*.info"))

