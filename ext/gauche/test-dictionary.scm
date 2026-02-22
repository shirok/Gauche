;;
;; test gauche.dictionary
;;

(use gauche.test)
(test-start "gauche.process")

(use gauche.dictionary)
(test-module 'gauche.dictionary)

(test-section "dictionary and circular data structure")
;; https://github.com/shirok/Gauche/pull/1223

(define-class <user-dict> (<dictionary>)
  ((value :init-keyword :value)))

(define-method write-object ((self <user-dict>) out)
  (format out "#<user-dict ~a>" (slot-ref self 'value)))

(define-method call-with-iterator ((self <user-dict>) proc :key :allow-other-keys)
  (call-with-iterator (list (cons 'value (slot-ref self 'value))) proc))

(define-method dict-transparent? ((self <user-dict>))
  #t)

(let1 dict (make <user-dict> :value 42)
  (test* "user defined dictionary"
         "#<user-dict 42>"
         (write-to-string dict))

  (test* "user defined dictionary (pprint)"
         "#<user-dict [1] ((value . 42))>\n"
         (call-with-output-string (cut pprint dict :port <>))))

(let* ([b (box #f)]
       [dict (make <user-dict> :value b)])
  (set-box! b dict)
  (test* "user defined dictionary (circular)"
         "#0=#<box #<user-dict #0#>>"
         (write-to-string b))

  (test* "user defined dictionary (pprint) (circular)"
         (write-to-string b)
         (call-with-output-string (cut pprint b :port <> :newline #f))))

(test-end)
