#! /usr/bin/env gosh

(add-load-path "../test")
(define *test-locale-dirs* '("../test/data/locale"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load test data.  this defines *tests*

(let ((test-file #`"data/gettext.data.,(gauche-character-encoding)"))
  (load test-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run the tests

(use gauche.test)
(use util.list)

(test-start "gettext")
(use text.gettext)
(test-module 'text.gettext)

(dolist (domain '("test" "motest"))
  (test-section domain)

  (bindtextdomain (list domain) *test-locale-dirs*)

  (dolist (locale '("en" "ja"))
    (let* ((gettext-dispatch (make-gettext domain locale))
           (get (gettext-dispatch 'getter)))
      (for-each
       (lambda (t)
         (test* (format "get-~A: ~S" locale (car t)) (get-optional (cdr t) (car t))
                (get (car t))))
       (assoc-ref *tests* locale)))) 

  ;; plural forms
  (dolist (locale '("en" "ja"))
    (let* ((gettext-dispatch (make-gettext domain locale))
           (nget (gettext-dispatch 'ngetter)))
      (for-each
       (lambda (t)
         (let ((msg (car t)) (msg2 (cadr t)))
           (for-each
            (lambda (t2)
              (test* (format "nget-~A: ~S (~D)" locale msg (car t2)) (cadr t2)
                     (format #f (nget msg msg2 (car t2)) (car t2))))
            (cddr t))))
       (assoc-ref *plural-tests* locale))))

  ;; using the GNU gettext interface
  (dolist (locale '("en" "ja"))
    (textdomain domain locale)
    (for-each
     (lambda (t)
       (test* (format "gettext-~A: ~S" locale (car t))
              (get-optional (cdr t) (car t))
              (gettext (car t)))
       (test* (format "dcgettext-~A: ~S" locale (car t))
              (get-optional (cdr t) (car t))
              (dcgettext domain (car t) locale)))
     (assoc-ref *tests* locale)))
  )

(test-end)

