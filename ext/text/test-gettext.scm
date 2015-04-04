(use gauche.test)
(use gauche.charconv)

(add-load-path "../../test")
(define *test-locale-dirs* '("../../test/data/locale"))

;; This hack is to avoid conversion errors due to supported encodings.
;; If you compile Gauche with utf8, most encodings should be OK.
(define *available-locales*
  (filter-map (^p (and (ces-conversion-supported? (car p) #f)
                       (cdr p)))
              '(("ISO-8859-15" . "en")
                ("eucJP" . "ja"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load test data.  this defines *tests*

(let1 test-file #"data/gettext.data.~(gauche-character-encoding)"
  (load test-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run the tests

(test-start "gettext")
(use text.gettext)
(test-module 'text.gettext)


(dolist [domain '("test" "motest")]
  (test-section domain)

  (bindtextdomain (list domain) *test-locale-dirs*)

  (dolist [locale *available-locales*]
    (let* ([gettext-dispatch (make-gettext domain locale)]
           [get (gettext-dispatch 'getter)])
      (for-each (^t (test* (format "get-~A: ~S" locale (car t))
                           (get-optional (cdr t) (car t))
                           (get (car t))))
                (assoc-ref *tests* locale))))

  ;; plural forms
  (dolist [locale *available-locales*]
    (let* ([gettext-dispatch (make-gettext domain locale)]
           [nget (gettext-dispatch 'ngetter)])
      (for-each
       (^t (let ([msg (car t)] [msg2 (cadr t)])
             (for-each
              (^[t2] (test* (format "nget-~A: ~S (~D)" locale msg (car t2))
                            (cadr t2)
                            (format #f (nget msg msg2 (car t2)) (car t2))))
              (cddr t))))
       (assoc-ref *plural-tests* locale))))

  ;; using the GNU gettext interface
  (dolist [locale *available-locales*]
    (textdomain domain locale)
    (for-each (^t (test* (format "gettext-~A: ~S" locale (car t))
                         (get-optional (cdr t) (car t))
                         (gettext (car t)))
                  (test* (format "dcgettext-~A: ~S" locale (car t))
                         (get-optional (cdr t) (car t))
                         (dcgettext domain (car t) locale)))
              (assoc-ref *tests* locale)))
  )

(test-end)

