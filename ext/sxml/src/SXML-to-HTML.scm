;			HTML Authoring in SXML
;
; The present file defines and demonstrates a function SXML->HTML, the
; most generic transformation of SXML into the corresponding HTML
; document. The SXML tree is traversed post-oder and converted into
; another tree, which, written in a depth-first fashion, results in a
; HTML document. The function SXML->HTML can generate an arbitrary
; HTML markup, for any existing or yet to be introduced HTML
; tag. Furthermore, the function supports one higher-level tag,
; 'html:begin'. As the source code below indicates, SXML->HTML can be
; trivially extended to support other higher-level tags.
;
; The proper HTML markup is being created by a set of node
; handlers. An iterator 'post-order' executes these functions while it
; traverses an SXML tree.
;
; Each node handler takes a tag (the head of an SXML node) and the
; list of children nodes, if any. A handler returns a fragment or a
; list of HTML fragments -- which become arguments to a handler of a
; parent SXML node.  A function SRV:send-reply takes the resulting
; tree of fragments and writes out the fragments in a depth-first
; order. The output is an HTML document that corresponds to the
; original SXML tree.
;
; This pretty-printing operation  makes it possible to author and
; compose HTML documents in their SXML form. SXML is more concise and
; expressive than a raw markup language. SXML representing regular
; Scheme code can be entered in any Scheme-sensitive editor. SXML as a
; data structure -- a list -- can likewise be composed as a literal or
; quasi-literal expression. Furthermore, SXML can be produced by regular
; Scheme functions, which may make authoring more succinct, advanced,
; and less tedious, as the code below illustrates.
;
; See SXML-tree-trans.scm for the definitions of SRV:send-reply and
; 'post-order' functions.
; See http://pobox.com/~oleg/ftp/Scheme/xml.html#XML-authoring
; for more examples and explanation.
;
; $ Id: SXML-to-HTML.scm,v 1.4 2001/09/17 20:20:33 oleg Exp oleg $

	; See http://pobox.com/~oleg/ftp/Scheme/myenv.scm
	; See http://pobox.com/~oleg/ftp/Scheme/myenv-scm.scm
	; See http://pobox.com/~oleg/ftp/Scheme/myenv-bigloo.scm
;(module HTML-authoring
;	(include "myenv-bigloo.scm")		; For use with Bigloo 2.2b
;	(include "SXML-tree-trans.scm"))
;(load "myenv-scm.scm")		; For use with SCM v5d2
(include "myenv.scm")		; For use with Gambit-C 3.0

(include "util.scm")

; You can find SXML-tree-trans.scm in the same directory (web site)
; where you found the present file
(include "SXML-tree-trans.scm")

; The following macro runs built-in test cases -- or does not run,
; depending on which of the two lines below you commented out
;(define-macro (run-test . body) `(begin (display "\n-->Test\n") ,@body))
(define-macro (run-test . body) '(begin #f))

; The following procedure is the most generic transformation of SXML
; into the corresponding HTML document. The SXML tree is traversed
; post-oder (depth-first) and transformed into another tree, which,
; written in a depth-first fashion, results in an HTML document.
 
(define (SXML->HTML tree)
 (SRV:send-reply
   (post-order tree
                ; Universal transformation rules. Work for every HTML,
                ; present and future
    `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) ((enattr attr-key) value))))
      . ,(lambda (trigger . value) (list '@ value)))
     (*default* . ,(lambda (tag . elems) (apply (entag tag) elems)))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodHTML str) str)))
 
                ; Handle a nontraditional but convenient top-level element:
                ; (html:begin title <html-body>) element
     (html:begin . ,(lambda (tag title . elems)
		      (assert (string? title))
        (list "Content-type: text/html"         ; HTTP headers
              "\n\n"                            ; two nl end the headers
              "<HTML><HEAD><TITLE>" title "</TITLE></HEAD>\n"
	      elems
              "</HTML>"))))
 
     )))

; The following two functions create the HTML markup for tags and attributes.
; They are being used in the node handlers for the post-order function, see
; above.

(define (entag tag)
  (lambda elems
    (if (and (pair? elems) (pair? (car elems)) (eq? '@ (caar elems)))
        (list #\< tag (cdar elems) #\>
              (and (pair? (cdr elems))
                   (list (cdr elems) "</" tag #\>)) #\newline)
        (list #\< tag #\> (and (pair? elems) (list elems "</" tag #\>))
                               #\newline))))
 
(define (enattr attr-key)
  (lambda (value)
    (if (null? value) (list #\space attr-key)
        (list #\space attr-key "=\"" value #\"))))


; Given a string, check to make sure it does not contain characters
; such as '<' or '&' that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate character entities.

(define string->goodHTML
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))))


; tests
(run-test
 (letrec ((gen (lambda (test-val)
		 (with-output-to-string
		   (lambda ()
		     (SXML->HTML
		      `(p "par1" "par2" 
			  ,(and test-val (list "par3" "par4")))))))
	      ))
   (write (gen #t))
   (assert (equal? "<p>par1par2par3par4</p>\n" (gen #t)))
   (assert (equal? "<p>par1par2</p>\n" (gen #f)))
   )

 (letrec ((gen (lambda (exp)
		(with-output-to-string
		  (lambda ()
		  (SXML->HTML exp))))))
  (assert (equal? "<p>&amp;</p>\n" (gen '(p "&"))))
  ;(write (gen '(p (@ (ALIGN "center")) "bad chars:" "<>&\"")))
  (assert (equal? "<p align=\"center\">bad chars:&lt;&gt;&amp;&quot;</p>\n"
		  (gen '(p (@ (align "center")) "bad chars:" "<>&\""))))
  (assert (equal? "<p align=\"center\" atr=\"&lt;value&gt;\">bad chars:<em>&lt;&gt;&amp;&quot;</em>\n</p>\n"
		  (gen '(p (@ (align "center") (atr "<value>"))
			   "bad chars:" (em "<>&\"")))))
  (assert (equal? "<p align=\"center\" atr=\"&quot;text&quot;\"><br>\n<ul compact><li>item 1</li>\n</ul>\n</p>\n"
		  (gen '(p (@ (align "center") (atr "\"text\"")) (br)
			   (ul (@ (compact)) (li "item " 1))))))
  (assert (equal? "<p><br>\n<ul compact><li>item 1</li>\n</ul>\n</p>\n"
		  (gen '(p (@) (br) (ul (@ (compact)) (li "item " 1))))))
  (assert (equal? "Content-type: text/html\n\n<HTML><HEAD><TITLE>my title</TITLE></HEAD>\n<body bgcolor=\"#ffffff\"><p>par1</p>\n</body>\n</HTML>"
      (gen 
	  '(html:begin "my title" (body (@ (bgcolor "#ffffff")) (p "par1"))))))
)

 (let ()
  (define (print-slide n max-count)
    (SXML->HTML
     `((h2 "Slide number:" ,n)     ; Note n is used in its native form
       ,(and (positive? n)
	     `(a (@ (href "base-url&slide=" ,(- n 1))) "prev"))
       ,(and (< (+ n 1) max-count)
	     `(a (@ (href "base-url&slide=" ,(+ n 1))) "next"))
       (p "the text of the slide"))))
  (assert (equal? "<h2>Slide number:0</h2>\n<p>the text of the slide</p>\n"
		  (with-output-to-string (lambda () (print-slide 0 1)))))
  (assert (equal? "<h2>Slide number:0</h2>\n<a href=\"base-url&amp;slide=1\">next</a>\n<p>the text of the slide</p>\n"
		  (with-output-to-string (lambda () (print-slide 0 3)))))
  (assert (equal? "<h2>Slide number:1</h2>\n<a href=\"base-url&amp;slide=0\">prev</a>\n<a href=\"base-url&amp;slide=2\">next</a>\n<p>the text of the slide</p>\n"
		  (with-output-to-string (lambda () (print-slide 1 3)))))
  (assert (equal? "<h2>Slide number:2</h2>\n<a href=\"base-url&amp;slide=1\">prev</a>\n<p>the text of the slide</p>\n"
		  (with-output-to-string (lambda () (print-slide 2 3)))))
  )

 (SXML->HTML
  `(ul
    ,@(map (lambda (filename-title)
	     `(li (a (@ (href ,(car filename-title))))
		  ,(cdr filename-title)))
	   '(("slides/slide0001.gif" . "Introduction")
	     ("slides/slide0010.gif" . "Summary")))
    )
  )
)
