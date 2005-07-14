;;
;; Test dbi modules
;;  $Id: dbi.scm,v 1.1 2005-07-14 09:11:19 shirok Exp $

(use gauche.test)
(use gauche.sequence)

(test-start "dbi/dbd")
(use dbi)
(test-module 'dbi)

(test-section "testing with dbd-null")

(let ((conn #f)
      (query #f)
      )
  (test* "dbi-connect (null)" '<null-connection>
         (begin (set! conn (dbi-connect "dbi:null"))
                (class-name (class-of conn))))

  (test* "dbi-prepare" #t
         (begin (set! query (dbi-prepare conn "select * from foo where x = ?"))
                (is-a? query <dbi-query>)))

  (test* "dbi-execute" '("select * from foo where x = 'z'")
         (coerce-to <list> (dbi-execute query "z")))
  )

(test-end)


