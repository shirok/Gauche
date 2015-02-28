;;;
;;; bcrypt - blowfish password hashing interface
;;;
;;;  This is a binding for a public-domain implementation of blowfish crypt
;;;  routine (crypt_blowfish).
;;;  I also put this file in public domain.
;;;

(define-module crypt.bcrypt
  (use gauche.uvector)
  (use math.mt-random)
  (export bcrypt-hashpw bcrypt-gensalt))
(select-module crypt.bcrypt)

(inline-stub
 "extern char *crypt_ra(const char *key, const char *setting,
                        void **data, int *size);
  extern char *crypt_gensalt_ra(const char *prefix, unsigned long count,
                                const char *input, int size);
  "

 (define-cproc crypt-ra (pass::<const-cstring> setting::<const-cstring>)
   (let* ([data::void* NULL] [size::int 0]
          [c::char* (crypt_ra pass setting (& data) (& size))])
     (when (== c NULL) (Scm_SysError "crypt_ra failed"))
     (let* ([r (SCM_MAKE_STR_COPYING c)])
       (free data)
       (return r))))

 (define-cproc crypt-gensalt-ra (prefix::<const-cstring>
                                 count::<ulong>
                                 randomsrc::<u8vector>)
   (let* ([c::char* (crypt_gensalt_ra prefix count
                                      (cast (const char*)
                                            (SCM_U8VECTOR_ELEMENTS randomsrc))
                                      (SCM_U8VECTOR_SIZE randomsrc))])
     (when (== c NULL) (Scm_SysError "crypt_gensalt_ra failed"))
     (let* ([r (SCM_MAKE_STR_COPYING c)])
       (free c)
       (return r))))
 )

(define (bcrypt-hashpw pass :optional (setting #f))
  (crypt-ra pass (or setting (bcrypt-gensalt))))

(define (bcrypt-gensalt :key (prefix "$2a$") (count 10) (entropy-source #f))
  (crypt-gensalt-ra prefix count (or entropy-source (get-entropy))))

(define-constant +esize+ 16)

(define (get-entropy)
  (or (call-with-input-file "/dev/urandom"
        (^p (and p
                 (rlet1 v (make-u8vector +esize+ 0)
                   (dotimes [i +esize+]
                     (set! (u8vector-ref v i) (read-byte p))))))
        :if-does-not-exist #f)
      (let1 mt (make <mersenne-twister> :seed (* (sys-time) (sys-getpid)))
        (rlet1 v (make-u8vector +esize+ 0)
          (dotimes [i +esize+]
            (set! (u8vector-ref v i) (mt-random-integer mt 256)))))))
