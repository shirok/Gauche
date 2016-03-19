(use gauche.test)
(use srfi-13)
(use gauche.uvector)
(test-start "bcrypt")

(use crypt.bcrypt)
(test-module 'crypt.bcrypt)

;; Test data is taken from wrapper.c
(define (test-hashpw hash pw)
  (test* "bcrypt-hashpw" hash (bcrypt-hashpw pw hash)))

(test-hashpw "$2a$05$CCCCCCCCCCCCCCCCCCCCC.E5YPO9kmyuRGyh0XouQYb4YMJKvyOeW"
             "U*U")
(test-hashpw "$2a$05$CCCCCCCCCCCCCCCCCCCCC.VGOzA784oUp/Z0DY336zx7pLYAy0lwK"
             "U*U*")
(test-hashpw "$2a$05$XXXXXXXXXXXXXXXXXXXXXOAcXxm9kjPGEMsLznoKqmqw7tc8WCx4a"
             "U*U*U")
(test-hashpw "$2a$05$abcdefghijklmnopqrstuu5s2v8.iXieOjg/.AySBTTZIIVFJeBui"
             "0123456789abcdefghijklmnopqrstuvwxyz\
              ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(test-hashpw "$2a$05$abcdefghijklmnopqrstuu5s2v8.iXieOjg/.AySBTTZIIVFJeBui"
	     "0123456789abcdefghijklmnopqrstuvwxyz\
	      ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\
	      chars after 72 are ignored")
(test-hashpw "$2x$05$/OK.fbVrR/bpIqNJ5ianF.CE5elHaaO4EbggVDjb8P19RukzXSM3e"
	     (u8vector->string #u8(#xa3)))
(test-hashpw "$2x$05$/OK.fbVrR/bpIqNJ5ianF.CE5elHaaO4EbggVDjb8P19RukzXSM3e"
	     (u8vector->string #u8(#xff #xff #xa3)))
(test-hashpw "$2y$05$/OK.fbVrR/bpIqNJ5ianF.CE5elHaaO4EbggVDjb8P19RukzXSM3e"
	     (u8vector->string #u8(#xff #xff #xa3)))
(test-hashpw "$2a$05$/OK.fbVrR/bpIqNJ5ianF.nqd1wy.pTMdcvrRWxyiGL2eMz.2a85."
	     (u8vector->string #u8(#xff #xff #xa3)))
(test-hashpw "$2b$05$/OK.fbVrR/bpIqNJ5ianF.CE5elHaaO4EbggVDjb8P19RukzXSM3e"
	     (u8vector->string #u8(#xff #xff #xa3)))
(test-hashpw "$2y$05$/OK.fbVrR/bpIqNJ5ianF.Sa7shbm4.OzKpvFnX1pQLmQW96oUlCq"
	     (u8vector->string #u8(#xa3)))
(test-hashpw "$2a$05$/OK.fbVrR/bpIqNJ5ianF.Sa7shbm4.OzKpvFnX1pQLmQW96oUlCq"
	     (u8vector->string #u8(#xa3)))
(test-hashpw "$2b$05$/OK.fbVrR/bpIqNJ5ianF.Sa7shbm4.OzKpvFnX1pQLmQW96oUlCq"
	     (u8vector->string #u8(#xa3)))
(test-hashpw "$2x$05$/OK.fbVrR/bpIqNJ5ianF.o./n25XVfn6oAPaUvHe.Csk4zRfsYPi"
	     (string-append "1" (u8vector->string #u8(#xa3)) "345"))
(test-hashpw "$2x$05$/OK.fbVrR/bpIqNJ5ianF.o./n25XVfn6oAPaUvHe.Csk4zRfsYPi"
	     (string-append (u8vector->string #u8(#xff #xa3)) "345"))
(test-hashpw "$2x$05$/OK.fbVrR/bpIqNJ5ianF.o./n25XVfn6oAPaUvHe.Csk4zRfsYPi"
	     (string-append (u8vector->string #u8(#xff #xa3)) "34"
	                    (u8vector->string #u8(#xff #xff #xff #xa3)) "345"))
(test-hashpw "$2y$05$/OK.fbVrR/bpIqNJ5ianF.o./n25XVfn6oAPaUvHe.Csk4zRfsYPi"
	     (string-append (u8vector->string #u8(#xff #xa3)) "34"
			    (u8vector->string #u8(#xff #xff #xff #xa3)) "345"))
(test-hashpw "$2a$05$/OK.fbVrR/bpIqNJ5ianF.ZC1JEJ8Z4gPfpe1JOr/oyPXTWl9EFd."
	     (string-append (u8vector->string #u8(#xff #xa3)) "34"
			    (u8vector->string #u8(#xff #xff #xff #xa3)) "345"))
(test-hashpw "$2y$05$/OK.fbVrR/bpIqNJ5ianF.nRht2l/HRhr6zmCp9vYUvvsqynflf9e"
	     (string-append (u8vector->string #u8(#xff #xa3)) "345"))
(test-hashpw "$2a$05$/OK.fbVrR/bpIqNJ5ianF.nRht2l/HRhr6zmCp9vYUvvsqynflf9e"
	     (string-append (u8vector->string #u8(#xff #xa3)) "345"))
(test-hashpw "$2a$05$/OK.fbVrR/bpIqNJ5ianF.6IflQkJytoRVc1yuaNtHfiuq.FRlSIS"
	     (string-append (u8vector->string #u8(#xa3)) "ab"))
(test-hashpw "$2x$05$/OK.fbVrR/bpIqNJ5ianF.6IflQkJytoRVc1yuaNtHfiuq.FRlSIS"
	     (string-append (u8vector->string #u8(#xa3)) "ab"))
(test-hashpw "$2y$05$/OK.fbVrR/bpIqNJ5ianF.6IflQkJytoRVc1yuaNtHfiuq.FRlSIS"
	     (string-append (u8vector->string #u8(#xa3)) "ab"))
(test-hashpw "$2x$05$6bNw2HLQYeqHYyBfLMsv/OiwqTymGIGzFsA4hOTWebfehXHNprcAS"
	     (u8vector->string #u8(#xd1 #x91)))
(test-hashpw "$2x$05$6bNw2HLQYeqHYyBfLMsv/O9LIGgn8OMzuDoHfof8AQimSGfcSWxnS"
	     (u8vector->string #u8(#xd0 #xc1 #xd2 #xcf #xcc #xd8)))
(test-hashpw "$2a$05$/OK.fbVrR/bpIqNJ5ianF.swQOIzjOiJ9GHEPuhEkvqrUyvWhEMx6"
	     (string-append
	      (u8vector->string
	       #u8(#xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa
		   #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa
		   #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa
		   #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa
		   #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa
		   #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa #xaa))
	      "chars after 72 are ignored as usual"))
(test-hashpw "$2a$05$/OK.fbVrR/bpIqNJ5ianF.R9xrDjiycxMbQE2bp.vgqlYpW5wx2yy"
	     (u8vector->string
	      #u8(#xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55
		  #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55
		  #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55
		  #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55
		  #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55
		  #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55)))
(test-hashpw "$2a$05$/OK.fbVrR/bpIqNJ5ianF.9tQZzcJfm3uj2NvJ/n5xkhpqLrMpWCe"
	     (u8vector->string
	      #u8(#x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff
		  #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff
		  #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff
		  #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff
		  #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff
		  #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff #x55 #xaa #xff)))
(test-hashpw "$2a$05$CCCCCCCCCCCCCCCCCCCCC.7uG0VCzI2bS7j6ymqJi9CdcdxiRTWNy"
	     "")

(define (test-wrong-hash reason hash)
  (test* #"bcrypt-hashpw wrong hash setting: ~reason"
	 (test-error)
	 (bcrypt-hashpw "" hash)))

(test-wrong-hash "iteration count is smaller than 4"
		 "$2a$03$CCCCCCCCCCCCCCCCCCCCC.")
(test-wrong-hash "iteration count is larger than 31" ;; can be up to 99, but currently limited to 31
		 "$2a$32$CCCCCCCCCCCCCCCCCCCCC.")
(test-wrong-hash "method 'c' is not implemented yet"
		 "$2c$05$CCCCCCCCCCCCCCCCCCCCC.")
(test-wrong-hash "method 'z' is not implemented yet"
		 "$2z$05$CCCCCCCCCCCCCCCCCCCCC.")
(test-wrong-hash "method is smaller than 'a'"
		 "$2`$05$CCCCCCCCCCCCCCCCCCCCC.")
(test-wrong-hash "method is larger than 'z'"
		 "$2{$05$CCCCCCCCCCCCCCCCCCCCC.")

(test* "bcrypt-gensalt" "$2b$10$"
       (string-take (bcrypt-gensalt) 7))
(test* "bcrypt-gensalt" "$2b$12$"
       (string-take (bcrypt-gensalt :count 12) 7))
(test* "bcrypt-gensalt 'a'" "$2a$12$"
       (string-take (bcrypt-gensalt :prefix "$2a$" :count 12) 7))
(test* "bcrypt-gensalt 'b'" "$2b$12$"
       (string-take (bcrypt-gensalt :prefix "$2b$" :count 12) 7))

(test* "bcrypt-gensalt count smaller than 4" (test-error)
       (bcrypt-gensalt :count 3))
(test* "bcrypt-gensalt count larger than 31" (test-error)
       (bcrypt-gensalt :count 32))

(test* "bcrypt-gensalt not implemented prefix 'c'" (test-error)
       (bcrypt-gensalt :prefix "$2c$"))
(test* "bcrypt-gensalt not implemented prefix 'z'" (test-error)
       (bcrypt-gensalt :prefix "$2z$"))
(test* "bcrypt-gensalt prefix smaller than 'a' (`)" (test-error)
       (bcrypt-gensalt :prefix "$2`$"))
(test* "bcrypt-gensalt prefix larger than 'z' ({)" (test-error)
       (bcrypt-gensalt :prefix "$2{$"))

(test-end)
