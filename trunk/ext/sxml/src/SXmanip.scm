;		XML processing in Scheme
;	    SXML to XML/HTML conversions and
;   SXSLT, XML Stylesheet Language Transformations in Scheme

; $ Id: SXmanip.scm,v 1.5 2001/02/27 23:43:39 oleg Exp oleg $

(include "myenv.scm")
(include "SXML-tree-trans.scm")

; Running examples: SXML data structures

(define tree1 
  '(html
    (head (title "Slides"))
    (body
     (p (@ (align "center"))
	(table (@ (style "font-size: x-large"))
	       (tr
		(td (@ (align "right")) "Talks ")
		(td (@ (align "center")) " = ")
		(td " slides + transition"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " data + control"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " programs"))))
     (ul
      (li (a (@ (href "slides/slide0001.gif")) "Introduction"))
      (li (a (@ (href "slides/slide0010.gif")) "Summary")))
     )))


(define tree2
  '(Forecasts (@ (TStamp "958082142"))
     (TAF (@ (TStamp "958066200") (LatLon "36.583, -121.850")
	     (BId "724915") (SName "KMRY, MONTEREY PENINSULA"))
	  (VALID (@ (TRange "958068000, 958154400")) "111730Z 111818")
	  (PERIOD (@ (TRange "958068000, 958078800"))
		  (PREVAILING "31010KT P6SM FEW030"))
	  (PERIOD (@ (TRange "958078800, 958104000") (Title "FM2100"))
		  (PREVAILING "29016KT P6SM FEW040"))
	  (PERIOD (@ (TRange "958104000, 958154400") (Title "FM0400"))
		  (PREVAILING "29010KT P6SM SCT200")
		  (VAR (@ (Title "BECMG 0708") (TRange "958114800, 958118400"))
		       "VRB05KT"))
)))

; Converting SXML to XML/HTML by direct evaluation of SXML,
; as if it were a directly executable code

(define (entag tag)
  (lambda elems
    (if (and (pair? elems) (pair? (car elems)) (eq? '@ (caar elems)))
	(list #\< tag (cdar elems) #\> (cdr elems) "</" tag #\> #\newline)
	(list #\< tag #\> elems "</" tag #\> #\newline))))
(define (enattr attr-key)
  (lambda (value)
    (if (eq? value #t) (list #\space attr-key)
	(list #\space attr-key "=\"" value #\"))))

(define html (entag "HTML"))
(define head (entag "HEAD"))
(define title (entag "TITLE"))
(define body (entag "BODY"))
(define p (entag "P"))
(define table (entag "TABLE"))
(define tr (entag "TR"))
(define td (entag "TD"))
(define ul (entag "UL"))
(define li (entag "LI"))
(define a (entag "A"))


(define (@ . elems) (cons '@ elems))
(define align (enattr "ALIGN"))
(define style (enattr "STYLE"))
(define href (enattr "HREF"))


; It's instructive to do the following:
;(pp (eval tree1))

(SRV:send-reply (eval tree1))

#|
(pp `(ul
      ,@(map (lambda (filename-title)
	       `(li (a (@ (href ,(car filename-title))))
		    ,(cdr filename-title)))
	     '(("slides/slide0001.gif" . "Introduction")
	       ("slides/slide0010.gif" . "Summary")))
      ))
|#

;------------------------------------------------------------------------
;			Tree traversals 
; tree is understood to be a grove as well:
; <tree> ::= <Node> | <Nodeset>
; where (see SXPath.scm)
; 	<Node> ::= (name <Node> ...) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodeset> ::= (<Node> ...)
; where 'name' is a symbol.

; The following three procedures are borrowed from SXPath.scm
(define (nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

	; Apply proc to each element of lst and return the list of results.
	; if proc returns a nodeset, splice it into the result
(define (map-union proc lst)
  (if (null? lst) lst
      (let ((proc-res (proc (car lst))))
	((if (nodeset? proc-res) append cons)
	 proc-res (map-union proc (cdr lst))))))

; The criterion 'crit' is a symbol, one of the following:
;	id		- tests if the Node has the right name (id)
;	@		- tests if the Node is an <attributes-coll>
;	*		- tests if the Node is an <Element>
;	*text*		- tests if the Node is a text node
;	*PI*		- tests if the Node is a PI node
;	*any*		- #t for any type of Node

(define (node-typeof? crit)
  (lambda (node)
    (case crit
      ((*) (and (pair? node) (not (memq (car node) '(@ *PI*)))))
      ((*any*) #t)
      ((*text*) (string? node))
      (else
       (and (pair? node) (eq? crit (car node))))
)))


; The following procedure is the most generic transformation of SXML
; into the corresponding XML document. The SXML tree is traversed
; post-oder (depth-first) and transformed into another tree, which,
; written in a depth-first fashion, results in an XML document.
; Unlike the '(eval tree1)' example above, there is no need to declare
; a specific binding for each tag beforehand. The computation below is
; however very similar to call-by-value. Yet there is a twist: an
; extension of bindings before argument expressions are evaluated.
; The procedure below processes all elements and attributes in an SXML
; tree uniformly. The example however can easily be amended to handle
; a certain element in a particular way, or to omit it from the
; resulting XML document.

(SRV:send-reply
 (post-order tree1
   `((@
      ((*default*	; local override for attributes
	. ,(lambda (attr-key . value) ((enattr attr-key) value))))
      . ,(lambda (trigger . value) (list '@ value)))
     (*default* . ,(lambda (tag . elems) (apply (entag tag) elems)))
     (*text* . ,(lambda (trigger str) str))
     ))
)


; Give the list of all href attributes in tree1
(pp
 (post-order tree1
   `((href ((*text* . ,(lambda (trigger str) str)))
	   . ,(lambda (trigger value) value))
     (*text* . , (lambda (trigger value) '()))
     (*default* . ,(lambda (trigger . value) value))))
)

(pp
 (post-order tree1
   `((href ((*text* . ,(lambda (trigger str) str)))
	   . ,(lambda (trigger . value) value))
     (*text* . , (lambda (trigger value) '()))
     (*default* . ,(lambda (trigger . values) (apply append values)))))
)


; Pre-order traversal of a tree and creation of a new tree:
;	apply-templates:: tree x <templates> -> <new-tree>
; where
; <templates> ::= (<template> ...)
; <template>  ::= (<node-test> <node-test> ... <node-test> . <handler>)
; <node-test> ::= an argument to node-typeof? above
; <handler>   ::= <tree> -> <new-tree>
;
; This procedure does a _normal_, pre-order traversal of an SXML
; tree.  It walks the tree, checking at each node against the list of
; matching templates.
; If the match is found (which must be unique, i.e., unambiguous), the
; corresponding handler is invoked and given the current node as an
; argument.  The result from the handler, which must be a <tree>,
; takes place of the current node in the resulting tree.
; The name of the function is not accidental: it resembles rather closely
; an 'apply-templates' function of XSLT.

(define (apply-templates tree templates)

		; Filter the list of templates. If a template does not
		; contradict the given node (that is, its head matches
		; the type of the node), chop off the head and keep the
		; rest as the result. All contradicting templates are removed.
  (define (filter-templates node templates)
    (cond
     ((null? templates) templates)
     ((not (pair? (car templates)))  ; A good template must be a list
      (filter-templates node (cdr templates)))
     (((node-typeof? (caar templates)) node)
      (cons (cdar templates) (filter-templates node (cdr templates))))
     (else
      (filter-templates node (cdr templates)))))

		; Here <templates> ::= [<template> | <handler>]
		; If there is a <handler> in the above list, it must
		; be only one. If found, return it; otherwise, return #f
  (define (find-handler templates)
    (and (pair? templates)
	 (cond
	  ((procedure? (car templates))
	   (if (find-handler (cdr templates))
	       (error "ambiguous template match"))
	   (car templates))
	  (else (find-handler (cdr templates))))))

  (let loop ((tree tree) (active-templates '()))
   ;(cout "active-templates: " active-templates nl "tree: " tree nl)
    (if (nodeset? tree)
	(map-union (lambda (a-tree) (loop a-tree active-templates)) tree)
	(let ((still-active-templates 
	       (append 
		(filter-templates tree active-templates)
		(filter-templates tree templates))))
	  (cond 
	   ;((null? still-active-templates) '())
	   ((find-handler still-active-templates) =>
	    (lambda (handler) (handler tree)))
	   ((not (pair? tree)) '())
	   (else
	    (loop (cdr tree) still-active-templates))))))
)


; Give the list of all td elements in tree1
(pp
 (apply-templates tree1
		  `((td . ,(lambda (node) node)))
))

; Give the list of all href attributes in tree1 that are children
; of 'a' elements
(pp
 (apply-templates tree1
		  `((a @ href . ,(lambda (node) node)))
))

; Give the list of all align attributes in tree1 that are children
; of 'td' elements
(pp
 (apply-templates tree1
		  `((td @ align . ,(lambda (node) node)))
))

; Give the list of all Title attributes in tree2 that are children
; of 'PERIOD' and 'VAR' elements
(pp
 (apply-templates tree2
  `((PERIOD @ Title *text* . ,(lambda (node) (list 'Title-PERIOD node)))
    (VAR @ Title *text* . ,(lambda (node) (list 'Title-VAR node))))
  ))




(include "SXPath.scm")

; XSLT processing in Scheme
; The following Scheme code transforms the tree2 above the same way
; as an XSL stylesheet
;	http://zowie.metnet.navy.mil/~spawar/JMV-TNG/XML/TC/taf.xsl
; does regarding the corresponding OMF XML document.
;
; This XSLT stylesheet is commented in part below, for easy reference.

; <xsl:template match="TAF">
;   <P><BR/></P>
;   <TABLE BGCOLOR="#CCCCCC" width="100%" cellpadding="3">
;   <TR>
;     <TD><xsl:value-of select="@SName"/></TD>
;     <TD>Id: <xsl:value-of select="@BId"/></TD>
;     <TD>[<xsl:value-of select="@LatLon"/>]</TD>
;   </TR></TABLE>

;   <TABLE width="100%" cellpadding="1" BORDER="1" RULES="none" FRAME="hsides">
;   <TR>
;   <TD WIDTH="20%">
;     <xsl:eval>conv_date(this.selectSingleNode('@TStamp').text)</xsl:eval>
;     <BR/>
;     <xsl:eval>conv_trange(this.selectSingleNode('VALID/@TRange').text)</xsl:eval>
;   </TD>
;   <TD>
;      <xsl:value-of select="VALID"/>
;      <xsl:apply-templates select="PERIOD/*"/>
;   </TD>
;   </TR>
;   </TABLE>
; </xsl:template>

; <xsl:template match="PERIOD/PREVAILING">
;    <DIV>
;    <xsl:attribute name="class">
;        <xsl:eval>period_class(this.selectSingleNode('../@TRange').text)</xsl:eval>
;    </xsl:attribute>
;    <xsl:if test="../@Title"><xsl:value-of select="../@Title"/> </xsl:if>
;    <xsl:value-of />
;    </DIV>
; </xsl:template>

; <xsl:template match="PERIOD/VAR">
;    <DIV class="var">
;    <xsl:value-of select="@Title"/> <xsl:value-of />
;    </DIV>
; </xsl:template>

; Scheme XSLT code follows. It does not need to resort to xsl:eval :
; regular Scheme backquote suffices. The Scheme code is also shorter,
; and well integrated.

(OS:putenv "TZ" "GMT")

		; trange-str is a string of two numbers (epoch secs) 
		; separated by a ", "
		; The following function returns the numbers as a pair
(define (conv-TRange trange-str)
  (call-with-input-string trange-str
    (lambda (port)
      (let* ((tstart (read port))
	     (delim (read-char port))
	     (tend (read port)))
	(if (and (eq? delim #\,) (number? tstart) (number? tend))
	    (cons tstart tend)
	    (error "Invalid Trange string: " trange-str))))))

 
(define (handle-TAF TAF-elem)
  (define time-current 958078810) ; should be (OS:time)
		; Handle PREVAILING grandchild of a TAF-elem
  (define (handle-Prevailing elem)
    (let ((trange (conv-TRange (car 
	     ((sxpath `((PERIOD  ((eq? ,elem))) @ TRange *text*))
	      TAF-elem)))))
      `(div (@ (class ,(if (<= (car trange) time-current (cdr trange))
			   "per_c" "per_nc")))
	    ,((sxpath `((PERIOD ((eq? ,elem))) @ Title *text*)) TAF-elem)
	    " "
	    ,((sxpath '(*text*)) elem))))

  `((p (br))
    (table (@ (bgcolor "#CCCCCC") (width "100%") (cellpadding "3"))
     (tr
      (td ,((sxpath '(@ SName *text*)) TAF-elem))
      (td "Id: " ,((sxpath '(@ BId *text*)) TAF-elem))
      (td "[" ,((sxpath '(@ LatLon *text*)) TAF-elem) "]")
      ))
    (table 
     (@ (width "100%") (cellpadding "1") (BORDER "1") (RULES "none")
	(FRAME "hsides"))
     (tr (td (@ (width "20%"))
	     ,(OS:cftime "%m-%d %H:%M"
	       (string->number 
		(car ((sxpath '(@ TStamp *text*)) TAF-elem))))
	     (br)
	     ,(let ((trange
		     (conv-TRange
		      (car ((sxpath '(VALID @ TRange *text*)) TAF-elem)))))
		(list (OS:cftime "%d %H:%M" (car trange))
		      (OS:cftime " - %d %H:%M" (cdr trange))))
	     )
	 (td ,((sxpath '(VALID *text*)) TAF-elem)
	     ,(apply-templates TAF-elem
		 `((PERIOD PREVAILING . ,handle-Prevailing)
		   (PERIOD VAR . ,(lambda (elem)
				    `(div (@ (class "var"))
					  ,((sxpath '(@ Title *text*)) elem)
					  " "
					  ,((sxpath '(*text*)) elem))))
		   ))
	     )
     )))
)


(cerr "\n\nResult:\n")
(pp
 (apply-templates tree2
		  `((TAF . ,handle-TAF))
		  ))

(cerr "\n\nIn HTML:\n")
(SRV:send-reply
 (post-order 
  (apply-templates tree2
		   `((TAF . ,handle-TAF))
		   )

   `((@
      ((*default*	; local override for attributes
	. ,(lambda (attr-key value) ((enattr attr-key) value))))
      . ,(lambda (trigger . value) (list '@ value)))
     (*default* . ,(lambda (tag . elems) (apply (entag tag) elems)))
     (*text* . ,(lambda (trigger str) str))
     ))
)
