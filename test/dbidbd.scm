;;
;; Test dbi modules
;;

(use gauche.test)
(use gauche.sequence)

(test-start "dbi/dbd")
(use dbi)
(test-module 'dbi)

(test-section "testing with dbd-null")

(let ([conn #f]
      [query #f])
  (test* "dbi-connect" '<null-connection>
         (begin (set! conn (dbi-connect "dbi:null"))
                (and (dbi-open? conn)
                     (class-name (class-of conn)))))
  (test* "dbi-close (<dbi-connection>)" #f
         (begin (dbi-close conn)
                (dbi-open? conn)))

  (test* "dbi-connect w/options" '("testdata;host=foo.biz;port=8088;noretry"
                                   (("testdata" . #t)
                                    ("host" . "foo.biz")
                                    ("port" . "8088")
                                    ("noretry" . #t))
                                   (:username "anonymous" :password "sesame"))
         (begin
           (set! conn (dbi-connect "dbi:null:testdata;host=foo.biz;port=8088;noretry"
                                   :username "anonymous" :password "sesame"))
           (list (ref conn 'attr-string)
                 (ref conn 'attr-alist)
                 (ref conn 'options))))

  (test* "dbi-prepare" '<dbi-query>
         (begin (set! query (dbi-prepare conn "select * from foo where x = ?"))
                (class-name (class-of query))))

  (test* "execute query" '("select * from foo where x = 'z'")
         (coerce-to <list> (dbi-execute query "z")))

  (test* "execute query" '("select * from foo where x = 333")
         (coerce-to <list> (dbi-execute query 333)))

  (test* "execute query" '("select * from foo where x = ''''")
         (coerce-to <list> (dbi-execute query "'")))

  (test* "dbi-do" '("insert into foo values(2,3)")
         (coerce-to <list> (dbi-do conn "insert into foo values (2, 3)")))

  (test* "dbi-do" '("insert into foo values('don''t know',NULL)")
         (coerce-to <list>
                    (dbi-do conn "insert into foo values (?, ?)" '()
                            "don't know" #f)))

  (test* "<dbi-parameter-error>" (test-error <dbi-parameter-error>)
         (dbi-execute (dbi-prepare (dbi-connect "dbi:null")
                                   "select * from foo where x = ?")
                      1 2))

  (test* "<dbi-parameter-error>" (test-error <dbi-parameter-error>)
         (dbi-execute (dbi-prepare (dbi-connect "dbi:null")
                                   "select * from foo where x = ?")))

  (test* "<dbi-parameter-error>" (test-error <dbi-parameter-error>)
         (dbi-execute (dbi-prepare (dbi-connect "dbi:null")
                                   "select * from foo where x = 3")
                      4))
  )

(test-section "testing conditions")

(test* "<dbi-nonexistent-driver-error>" "nosuchdriver"
       (guard (e [(<dbi-nonexistent-driver-error> e)
                  (ref e 'driver-name)])
         (dbi-connect "dbi:nosuchdriver")))




(test-end)


