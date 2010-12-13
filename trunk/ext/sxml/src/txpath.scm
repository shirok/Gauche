;; SXPath processor for string representation of W3C XPath/XPointer expressions
; $Id: txpath.scm,v 1.1 2003-07-22 11:22:11 shirok Exp $:
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; XPointer's points and ranges are NOT implemented
;
; Full XPath Core Function Library is now supported. That is:
; 4.1 Node Set Functions
;    number last()
;    number position()
;    number count(node-set)
;    node-set id(object)
;    string local-name(node-set?)
;    string namespace-uri(node-set?)
;    string name(node-set?)
; 4.2 String Functions
;    string string(object?)
;    string concat(string, string, string*)
;    boolean starts-with(string, string)
;    boolean contains(string, string)
;    string substring-before(string, string)
;    string substring-after(string, string)
;    string substring(string, number, number?)
;    number string-length(string?)
;    string normalize-space(string?)
;    string translate(string, string, string)
; 4.3 Boolean Functions
;    boolean boolean(object)
;    boolean not(boolean)
;    boolean true()
;    boolean false()
;    boolean lang(string)
; 4.4 Number Functions
;    number number(object?)
;    number sum(node-set)
;    number floor(number)
;    number ceiling(number)
;    number round(number)

;=========================================================================
; Errors handling 

(define (sxml:xpointer-parse-error . text)
  (apply cerr (append (list nl "XPointer parser error: ") text (list nl)))
  #f)

; A warning message for grammar features which are not supported by this
; implementation
(define (sxml:xpointer-parse-warning . text)
  (apply cerr (append (list nl "XPointer parser warning: ") text (list nl))))

; Runtime errors handler (unbound variable, bad argument, etc).
; It may be re-defined (say, like a warning) without 'exit',  and evaluation will 
; be continued.
; In this case, a default value (usually empty nodeset or 0) is returned by 
; a sub-expression which caused an XPointer runtime error.
(define (sxml:xpointer-runtime-error . text)
  (apply cerr (append (list nl "XPointer runtime error: ") text (list nl)))
  (exit -1))

;=========================================================================
; Low level parsing functions
; The XPoiner path is represented as a list of chars

; A list of whitespace characters
(define sxml:whitespace '(#\space #\return #\newline #\tab))

; A sxml:whitespace or () <> [] : / + * , = | ! " ' @ $
(define sxml:delimiter (append sxml:whitespace
                              '(#\( #\) #\< #\> #\[ #\] #\: #\/ #\+ 
                                #\* #\, #\= #\| #\! #\" #\' #\@ #\$)))

; A list of characters a NCName cannot start with
(define (sxml:non-first? ch)
  (or (char-numeric? ch)
      (memv ch sxml:delimiter) 
      (memv ch '(#\. #\-))))

; The function reads a whitespace , production [3] (S) in XML Rec.
;  path - xpointer path string as a list of chars 
; It returns a new path
(define (sxml:skip-ws path)
  (if (or (null? path)
	  (not (memv (car path) sxml:whitespace)))
    path
    (sxml:skip-ws (cdr path))))

;------------------------------------------------
; These two functions read expected information from the path

; Whether the path begins with a 'str' (starting whitespaces are ignored)
;  str - a string to match
;  path - an xpointer path represented as a list of chars
;  char-list - an optional argument. If this argument is supplied, a 'str'
; pattern must be followed by a character from a 'char-list'
; If 'str' is really in the beginning of path, a new path is returned
; Otherwise, function returns #f (path remains unchanged)
(define (sxml:parse-check str path . char-list)
  (let loop ((lst (string->list str)) 
             (p (sxml:skip-ws path)))
    (cond
      ((null? lst)
       (if
        (or (null? p) (null? char-list) (memv (car p) (car char-list)))
        p
        #f))
      ((null? p) #f)
      ((char=? (car lst) (car p))
       (loop (cdr lst) (cdr p)))
      (else #f))))

; Checks whether the PATH starts with a sequence of strings (possibly
; separated by a whitespace) from STR-SEQ
; Returns a new PATH (match successful) or #f (otherwise)
(define (sxml:parse-check-sequence str-seq path . char-list)
  (let ((char-list (if (null? char-list) #f (car char-list))))
    (let loop ((str-seq str-seq)
               (path path))
      (cond
        ((null? str-seq) path)  ; successful match
        ((if char-list
             (sxml:parse-check (car str-seq) path char-list)
             (sxml:parse-check (car str-seq) path))
         => (lambda (new-path)
              (loop (cdr str-seq) new-path)))
        (else #f)))))  ; unsuccessful match

; Similar to the 'parse-check' function. But this function also has a side
; effect. It displays an error message if the 'str' doesn't match the beginning
; of 'path'.
(define (sxml:parse-assert str path)
  (let loop ((lst (string->list str)) 
	     (p (sxml:skip-ws path)))
    (cond
      ((null? lst) p)
      ((null? p) 
       (sxml:xpointer-parse-error 
        "unexpected end of XPointer path. "
        "Expected - \"" str "\", given - \"" (list->string path) "\""))
      ((char=? (car lst) (car p)) (loop (cdr lst) (cdr p)))
      (else
       (sxml:xpointer-parse-error
        "expected - \"" str "\", given - \"" (list->string path) "\"")))))

             
;------------------------------------------------
; NCName readers

; Reads a NCName, taking into account that whitespaces and characters:
; ( ) < > [ ] : / + * , = | ! " ' @ $
; may not be used in it.
; Moreover, its first character can't be: . - or a digit
; The result:  (list  ncname  new-path)
;          or  #f
;  ncname - NCName represented as a string
; If there is no NCName in the current position of the path, then an error 
; message is displayed and #f is returned
(define (sxml:parse-ncname path)
  (let((path (sxml:skip-ws path)))
    (cond
      ((null? path) 
       (sxml:xpointer-parse-error
        "unexpected end of XPointer path. Expected - NCName"))
      ((sxml:non-first? (car path))
       (sxml:xpointer-parse-error
        "expected - NCName instead of " (car path)))
      (else
       (let loop ((ncname (list (car path)))
                  (path (cdr path)))
         (cond
           ((null? path) (list (list->string (reverse ncname)) path))
           ((memv (car path) sxml:delimiter)           
            (list (list->string (reverse ncname)) path))
           (else (loop (cons (car path) ncname) (cdr path)))))))))

; Reads a Name production. It is similar to a 'parse-ncname' function.
; The only difference is that #\: is allowed within a Name
(define (sxml:parse-name path)
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error
	 "unexpected end of XPointer path. Expected - Name"))
      ((and (sxml:non-first? (car path))
	    (not (char=? (car path) #\:)))
       (sxml:xpointer-parse-error "expected - Name instead of " (car path)))
      (else (let loop ((ncname (list (car path)))
		       (path (cdr path)))
	      (cond
		((null? path) 
		 (list (list->string (reverse ncname)) path))
		((and (memv (car path) sxml:delimiter)
		      (not (char=? (car path) #\:)))
		 (list (list->string (reverse ncname)) path))
		(else (loop (cons (car path) ncname) (cdr path)))))))))

; The function reads a qualified name (QName)
; Returns: ( (prefix . local-part) new-path )
;      or  ( local-part new-path )    if there is no prefix
;       if there is not QName in the beginning of the 'path' it calls 
;          sxml:xpointer-parse-error
;  prefix, local-part - strings
;  new-path - a list of characters
(define (sxml:parse-qname path)
  (and-let* ((r1 (sxml:parse-ncname path)))
	    (let ((first (car r1))
		  (path2 (cadr r1)))
	      (cond
		((null? path2) (list first path2))
		((not (char=? (car path2) #\:)) (list first path2))
		((null? (cdr path2))
		 (sxml:xpointer-parse-error "no local part of a qualified name"))
		((char=? (cadr path2) #\:) (list first path2))
		(else (and-let* ((r2 (sxml:parse-ncname (cdr path2))))
				(list (cons first (car r2)) (cadr r2)))
		      )))))
                   
;------------------------------------------------
; Parsers for data of basic types

; Reads a natural number:
; [1-9] [0-9]*
; The result:  (list  number  new-path)  or  #f
(define (sxml:parse-natural path)
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error
        "unexpected end of XPointer path. Expected - number"))
      ((or (char<? (car path) #\1) (char>? (car path) #\9))
       (sxml:xpointer-parse-error "expected - number instead of " (car path)))
      (else (let loop ((res (- (char->integer (car path))
			  48)) ; (char->integer #\0)
                  (path (cdr path)))
         (cond
           ((null? path) (list res path))
           ((char-numeric? (car path))
            (loop (+ (* res 10) (- (char->integer (car path)) 
				   48)) ; (char->integer #\0)
                  (cdr path)))
           (else (list res path))))))))

; Reads a Literal ([29] in XPath specification)
; [29]    Literal    ::=    '"' [^"]* '"'  
;                           | "'" [^']* "'"
; The result:  (string new-path)  or  #f
(define (sxml:parse-literal path)
  (let ((ch (if (sxml:parse-check "\"" path) #\" #\')))
    (let loop ((res '())
	       (path (sxml:parse-assert (if (char=? ch #\") "\"" "'") 
				       path)))
      (cond
	((not path) #f)
	((null? path)
	 (sxml:parse-assert (if (char=? ch #\") "\"" "'") 
			   path)
	 #f)
	((char=? (car path) ch)
	 (list (list->string (reverse res))
	       (cdr path)))
	(else (loop (cons (car path) res) (cdr path)))))))

; Reads a Number ([30]-[31] in XPath specification)
; [30]    Number    ::=    Digits ('.' Digits?)?  
;                          | '.' Digits  
; [31]    Digits    ::=    [0-9]+
; The result:  (number new-path)  or  #f
(define (sxml:parse-number path) 
  (define (digits path)
    (let loop ((n-lst '())
               (path path))
      (cond
        ((and (null? path) (null? n-lst))
         (sxml:xpointer-parse-error 
          "unexpected end of XPointer path. Expected - number"))
        ((null? path) (list n-lst path))
        ((and (or (char<? (car path) #\0) (char>? (car path) #\9))
              (null? n-lst))       
         (sxml:xpointer-parse-error "expected - number instead of " (car path)))
        ((or (char<? (car path) #\0) (char>? (car path) #\9))
         (list n-lst path))
        (else
         (loop (cons (- (char->integer (car path)) (char->integer #\0)) n-lst)
               (cdr path))))))
    
  (let ((path (sxml:skip-ws path)))
    (cond
      ((null? path)
       (sxml:xpointer-parse-error 
        "unexpected end of XPointer path. Expected - number"))
      ((char=? (car path) #\.)
       (and-let* ((lst (digits (cdr path))))
            (let rpt ((res 0)
                      (n-lst (car lst))
                      (path (cadr lst)))
              (if(null? n-lst)
                 (list (/ res 10) path)
                 (rpt (+ (/ res 10) (car n-lst))
                      (cdr n-lst) 
                      path)))))
      (else (and-let* ((lst (digits path)))
		      (let loop ((num1 0)
				 (n-lst (reverse (car lst)))
				 (path (cadr lst)))
			(if (null? n-lst)
			  (cond
			    ((null? path) (list num1 path))
			    ((not (char=? (car path) #\.)) (list num1 path))
			    (else
			      (and-let* ((lst2 (digits (cdr path))))
					(let rpt ((num2 0)
						  (n-lst (car lst2))
						  (path (cadr lst2)))
					  (if (null? n-lst)
					    (list (+ num1 (/ num2 10)) path)
					    (rpt (+ (/ num2 10) (car n-lst))
						 (cdr n-lst) 
						 path))))))
			  (loop (+ (* num1 10) (car n-lst))
				(cdr n-lst) 
				path))))))))

;=========================================================================
; Grammar parsing functions
; All these functions have similar arguments:
;  path - an xpointer path represented as a list of chars
;  ns-binding - declared namespace prefixes (not for all functions)
; ns-binding = (list (prefix . uri) (prefix . uri) ... )
; prefix, uri - strings

;------------------------------------------------
; Functions which parse XPath grammar

; Parses an AxisSpecifier production ([5],[6],[13] in XPath specification)
; [5]    AxisSpecifier    ::=    AxisName '::'  
;                                | AbbreviatedAxisSpecifier
; [6]    AxisName    ::=    'ancestor'  
;                           | 'ancestor-or-self'  
;                           | 'attribute'  
;                           | 'child'  
;                           | 'descendant'  
;                           | 'descendant-or-self'  
;                           | 'following'  
;                           | 'following-sibling'  
;                           | 'namespace'  
;                           | 'parent'  
;                           | 'preceding'  
;                           | 'preceding-sibling'  
;                           | 'self' 
; [13]    AbbreviatedAxisSpecifier    ::=    '@'? 
;
; The result is represented as a list
; (list  (lambda ...)  new-path  root-node-required)  
; or  #f in case of parse error
;  new-path - a list of chars. It represents the remainder of the source string
;  root-node-required - a boolean value
;  (lambda ...) one of the axis functions
; If root-node-required = #t, lambda's signature is
;  (lambda (test-pred?)
;   (lambda (root-node)
;    (lambda (nodeset) ... )))
; otherwise
;  (lambda (test-pred?)
;   (lambda (nodeset) ... ))
(define (sxml:parse-axis-specifier path)
  (let loop
    ((triples
      `((("ancestor" "::") ,sxml:ancestor #t)
        (("ancestor-or-self" "::") ,sxml:ancestor-or-self #t)
        (("attribute" "::") ,sxml:attribute #f)
        (("child" "::") ,sxml:child #f)
        (("descendant" "::") ,sxml:descendant #f)
        (("descendant-or-self" "::") ,sxml:descendant-or-self #f)
        (("following" "::") ,sxml:following #t)
        (("following-sibling" "::") ,sxml:following-sibling #t)
        (("namespace" "::") ,sxml:namespace #f)
        (("parent" "::") ,sxml:parent #t)
        (("preceding" "::") ,sxml:preceding #t)
        (("preceding-sibling" "::") ,sxml:preceding-sibling #t)
        (("self" "::") ,sxml:filter #f)
        (("@") ,sxml:attribute #f))))
    (if
     (null? triples)  ; a default (child) axis
     (list sxml:child path #f)
     (let rpt ((pattern (caar triples))
               (p path))
       (cond
         ((null? pattern)  ; match is successful
          (list (cadar triples) p (caddar triples)))
         ((sxml:parse-check (car pattern) p)
          => (lambda (new-path)
               (rpt (cdr pattern) new-path)))
         (else  ; match unsuccessful
          (loop (cdr triples))))))))


; Parses a NodeTest production 
; ([7],[37] in XPath specification, [11] in XPointer specification)
; [7]    NodeTest    ::=    NameTest  
;                           | NodeType '(' ')'  
;                           | 'processing-instruction' '(' Literal ')' 
; [37]    NameTest    ::=    '*'  
;                            | NCName ':' '*'  
;                            | QName  
; [11]   NodeType   ::=   'comment'  
;                         | 'text'  
;                         | 'processing-instruction'  
;                         | 'node'
;                         | 'point'
;                         | 'range'
; The result is:   ( (lambda (node) ...)  new-path )    or  #f
; #f signals of an error
;  (lambda (node) ...)  - a node test function
(define (sxml:parse-node-test path ns-binding)
  
  (define (brackets path)
    (and-let* ((path (sxml:parse-assert "(" path)))
         (sxml:parse-assert ")" path)))
  (cond
    ((sxml:parse-check-sequence '("comment" "(") path)
     => (lambda (path)
          (and-let*
           ((path (sxml:parse-assert ")" path)))
           (list (ntype?? '*COMMENT*) path))))
    ((sxml:parse-check-sequence '("text" "(") path)
     => (lambda (path)
          (and-let*
           ((path (sxml:parse-assert ")" path)))
           (list (ntype?? '*text*) path))))
    ((sxml:parse-check-sequence '("node" "(") path)
     => (lambda (path)
          (and-let*
           ((path (sxml:parse-assert ")" path)))
           (list sxml:node? path))))
    ((sxml:parse-check-sequence '("processing-instruction" "(") path)
     => (lambda (path)               
          (cond
            ((sxml:parse-check ")" path)
             (list (lambda (node)
                     (and (pair? node) (eq? (car node) '*PI*)))
                   (sxml:parse-assert ")" path)))
            (else
             (and-let*
              ((lst (sxml:parse-literal path))
               (name (string->symbol (car lst)))
               (path (sxml:parse-assert ")" (cadr lst))))
              (list
               (lambda (node)
                 (and (pair? node) 
                      (eq? (car node) '*PI*)
                      (equal? (cadr node) name)))
               path))))))
    ((sxml:parse-check-sequence '("point" "(") path)
     => (lambda (path)
          (and-let*
           ((path (sxml:parse-assert ")" path)))
           (sxml:xpointer-parse-warning 
            "'point()' NodeTest is not supported by this implementation. "
            "It is defaulted to an predicate which is always false")
           (list (lambda (node) #f) path))))
    ((sxml:parse-check-sequence '("range" "(") path)
     => (lambda (path)
          (and-let*
           ((path (sxml:parse-assert ")" path)))
           (sxml:xpointer-parse-warning 
            "'range()' NodeTest is not supported by this implementation. "
	     "It is defaulted to an predicate which is always false")
           (list (lambda (node) #f) path))))
    ((sxml:parse-check "*" path)
       (list (ntype?? '*) (sxml:parse-assert "*" path)))
    (else  ; NCName ':' '*'  |  QName
     (and-let*
      ((lst (sxml:parse-ncname path)))
      (let ((name (string->symbol (car lst)))
            (path (cadr lst)))
        (if
         (and (not (null? path)) (char=? (car path) #\:))
         (let ((path (sxml:parse-assert ":" path))
               (pair (assq name ns-binding)))
           (cond
             ((not pair) 
              (sxml:xpointer-parse-error "unknown namespace prefix - " name))
             ((and (not (null? path)) (char=? (car path) #\*))
              (list
               (ntype-namespace-id?? (cdr pair))
               (sxml:parse-assert "*" path)))
             (else
              (and-let*
               ((lst (sxml:parse-ncname path)))
               (list
                (ntype?? (string->symbol
                          (string-append (cdr pair) ":" (car lst))))
                (cadr lst))))))
         (list (ntype?? name) path)))))))  ; just a local name
         
;------------------------------------------------
; Functions which parse XPath grammar, part 2
;
; All these functions return a value which has the same signature:
; (list (lambda (nodeset root-node context var-binding) ...)
;       path 
;       index-required )
; or #f
; #f signals of a parse error (error message is printed as a side effect
; during parsing)
;
; (lambda (nodeset root-node context var-binding) - an SXPath-like 
; function (it transforms a nodeset into a new nodeset)
;  nodeset - a current set of nodes
;  root-node - the root of a document (a singleton nodeset)
;  context - the context of the node; list of two elements - (position size)
;  position - context position (a number)
;  size - context size (a number)
;  path - an XPointer path represented as the list of chars
;  index-required - boolean value: whether an id-index was required "deeper" 
; within the grammar

; Filter nodeset using preds-list as described in XPath rec. 2.4
; A helper for sxml:parse-step and sxml:parse-filter-expr
(define (sxml:xpath-nodeset-filter preds-list nodeset root-node 
				  var-binding)
  (let rpt ((nodeset nodeset)
	    (ps (reverse preds-list)))
    (if (null? ps) 
      nodeset
      (let ((size (length nodeset)))
	(let lab ((nset nodeset)
		  (res '())
		  (pos 1)) 
	  (if (null? nset)
	    (rpt (reverse res) (cdr ps))
	    (let ((val ((car ps) 
			(list (car nset)) 
			root-node 
			(cons pos size) 
			var-binding)))
	      (lab (cdr nset)
		   (if (if (number? val)
			 (= val pos)
			 (sxml:boolean val))
		     (cons (car nset) res)
		     res)
		   (+ pos 1))
	      )))))))

; Parses a Step production 
; ([4xptr] in XPointer specification, [12] in XPath specification)
; [4xptr] Step ::= AxisSpecifier NodeTest Predicate*
;                  | AbbreviatedStep
;                  | 'range-to' '(' Expr ')' Predicate*
; [12]    AbbreviatedStep    ::=    '.'  
;                                   | '..' 
; ATTENTION: 'range-to' FUNCTION IS NOT IMPLEMENTED. INFORMATION REFERRING
; TO IT IS PARSED AND IGNORED. THIS FUNCTION CALL RESULTS TO AN EMPTY NODESET
(define (sxml:parse-step path ns-binding)
  (cond
    ((sxml:parse-check ".." path)
     (list
      (lambda (nodeset root-node context var-binding)
        (((sxml:parent sxml:node?) root-node) nodeset))
      (sxml:parse-assert ".." path)
      #f))
    ((sxml:parse-check "." path)
     (list
      (lambda (nodeset root-node context var-binding)
        ((sxml:filter sxml:node?) nodeset))
      (sxml:parse-assert "." path)
      #f))
    ((sxml:parse-check "range-to" path)
     (and-let*
       ((path0 (sxml:parse-assert "(" (sxml:parse-assert "range-to" path)))
        (lst (sxml:parse-expr path0 ns-binding))
        (path (sxml:parse-assert ")" (cadr lst)))) ; check again?
       (let loop ((path path))
	 (cond
	   ((sxml:parse-check "[" path)
	    (and-let* ((lst (sxml:parse-predicate path ns-binding)))
		      (loop (cadr lst))))
	   (else   ; Predicates are over
	     (sxml:xpointer-parse-warning
	       "range-to function not implemented. "
	       "Defaulting to an empty nodeset")
	     (list
	       (lambda (nodeset root-node context var-binding)
		 '())
	       path
	       #f))))))
    (else (and-let* ((lst (sxml:parse-axis-specifier path)))
        (let ((axis (car lst))             
             (root-node-required (caddr lst)))
          (and-let* ((lst (sxml:parse-node-test (cadr lst) ns-binding)))
             (let ((test (car lst)))                 
               (let loop ((preds '())
                          (path (cadr lst))
                          (index-required #f))
                 (cond
                  ((sxml:parse-check "[" path)
                   (and-let* ((lst (sxml:parse-predicate path ns-binding)))
                        (loop (cons (car lst) preds)
                              (cadr lst)
                              (or index-required (caddr lst)))))
                  ; No more predicates 
                  ((null? preds)
                   (list
                    (lambda (nodeset root-node context var-binding)
                      (if root-node-required
                          (((axis test) root-node) nodeset)
                          ((axis test) nodeset)))
                    path
                    index-required))
                  (else  ; More predicates 
		    (list  
		      (lambda (nodeset root-node context var-binding)
			(sxml:xpath-nodeset-filter 
			  preds 
			  ((if root-node-required
			     ((axis test) root-node)
			     (axis test)) 
			   nodeset)
			  root-node var-binding))
		      path
		      index-required))
		  )))))))))

; Parses a RelativeLocationPath production ([3],[11] in XPath specification)
; [3]  RelativeLocationPath  ::=  Step  
;                                 | RelativeLocationPath '/' Step  
;                                 | AbbreviatedRelativeLocationPath 
; [11]  AbbreviatedRelativeLocationPath  ::=  RelativeLocationPath '//' Step 
(define (sxml:parse-relative-location-path path ns-binding)
  (let loop ((funcs '())
             (path path)
             (index-required #f))
    (and-let* ((lst (sxml:parse-step path ns-binding)))
         (let ((func (car lst))
               (path (cadr lst)))
           (cond
             ((sxml:parse-check "//" path)
              (loop
               (cons 
                (lambda (nodeset root-node context var-binding)
                  ((sxml:descendant-or-self sxml:node?) nodeset))
                (cons func funcs))
               (sxml:parse-assert "//" path)
               (or index-required (caddr lst))))
             ((sxml:parse-check "/" path)
              (loop (cons func funcs)
                    (sxml:parse-assert "/" path)
                    (or index-required (caddr lst))))
             ; no more steps
             ((null? funcs) lst) ; a current step is the only step
             (else   ; several steps   
              (list
               (lambda (nodeset root-node context var-binding)
                 (let rpt ((nset nodeset)
                           (fs (reverse (cons func funcs))))
                   (if (null? fs)
                      nset
                      (rpt ((car fs)
                            nset root-node context var-binding)
                           (cdr fs)))))
               path
               (or index-required (caddr lst)))))))))

; Parses XPath grammar
(define (sxml:parse-xpath path ns-binding)
  (and-let*
   ((res (sxml:parse-location-path path ns-binding))
    (path (sxml:skip-ws (cadr res))))
   (if
    (not (null? path))
    (sxml:xpointer-parse-error "unexpected - \"" (list->string path) "\"")
    res)))
                   
; Parses a LocationPath production ([1],[2],[10] in XPath specification)
; [1]    LocationPath    ::=    RelativeLocationPath  
;                               | AbsoluteLocationPath  
; [2]    AbsoluteLocationPath    ::=    '/' RelativeLocationPath?  
;                                       | AbbreviatedAbsoluteLocationPath
; [10]    AbbreviatedAbsoluteLocationPath    ::=    '//' RelativeLocationPath
(define (sxml:parse-location-path path ns-binding) 
  (define (nothing? path)
    (let ((path (sxml:skip-ws path)))
      (cond
	((null? path) #t)
	((memv (car path) '(#\| #\+ #\- #\< #\> #\= #\) #\] #\,)) #t)
	((or (sxml:parse-check "mod" path sxml:delimiter)
	     (sxml:parse-check "div" path sxml:delimiter)
	     (sxml:parse-check "!=" path)
	     (sxml:parse-check "and" path sxml:delimiter)
	     (sxml:parse-check "or" path sxml:delimiter)) #t)
	(else #f))))
  (cond
    ((sxml:parse-check "//" path)
     (and-let* ((lst (sxml:parse-relative-location-path 
		       (sxml:parse-assert "//" path)
		       ns-binding)))
	       (let ((func (car lst))
		     (path (cadr lst))
		     (index-required (caddr lst)))
		 (list (lambda (nodeset root-node context var-binding)
			 (func
			   ((sxml:descendant-or-self sxml:node?) root-node)
			   root-node context var-binding))
		       path
		       index-required))))
    ((sxml:parse-check "/" path)
     (let ((path (sxml:parse-assert "/" path)))
       (if (nothing? path)
	 (list (lambda (nodeset root-node context var-binding)
		 root-node)
	       path
	       #f)
	 (let ((lst (sxml:parse-relative-location-path path ns-binding)))
	   (if (null? lst)
	     #f
	     (let((func (car lst))
		  (path (cadr lst))
		  (index-required (caddr lst)))
	       (list (lambda (nodeset root-node context var-binding)
		       (func root-node root-node context var-binding))
		     path
		     index-required)))))))
    (else (sxml:parse-relative-location-path path ns-binding))))

; Parses a Predicate production ([8]-[9] in XPath specification)
; [8]    Predicate    ::=    '[' PredicateExpr ']'  
; [9]    PredicateExpr    ::=    Expr 
; Note that (according to specification) a Predicate must return a number or
; a boolean value. However, the return value type is not checked in this
; function. This is performed in functions that use 'parse-predicate'
(define (sxml:parse-predicate path ns-binding)
  (and-let* ((path0 (sxml:parse-assert "[" path))
             (lst (sxml:parse-expr path0 ns-binding))
             (path (sxml:parse-assert "]" (cadr lst))))
                 (list (car lst) ; func
		       path 
		       (caddr lst) ; index required
		       )))

; Parses a VariableReference production ([36] in XPath specification)
; [36]    VariableReference    ::=    '$' QName 
(define (sxml:parse-variable-reference path ns-binding)
  (and-let*
   ((path (sxml:parse-assert "$" path))
    (lst (sxml:parse-qname path)))
   (let ((name
          (string->symbol
           (if (pair? (car lst))  ; contains a prefix-part
               (string-append (caar lst) ":" (cdar lst))                                  
               (car lst)))))
     (list (lambda (nodeset root-node context var-binding)
             (cond
               ((assoc name var-binding)
                => cdr)
               (else (sxml:xpointer-runtime-error
                      "Unbound variable - " name
                      ". Defaulting to an empty nodeset")
                     '())))
           (cadr lst)
           #f))))
                         
; Parses a FunctionCall production ([16],[17],[35] in XPath specification)
; [16]    FunctionCall    ::=    FunctionName 
;                                '(' ( Argument ( ',' Argument )* )? ')'  
; [17]    Argument    ::=    Expr 
; [35]    FunctionName    ::=    QName - NodeType
; ATTENTION: SOME CORE FUNCTIONS ARE NOT SUPPORTED.
; SUCH A FUNCTION CALL IS DEFAULTED TO AN EMPTY NODESET
(define (sxml:parse-function-call path ns-binding)
  
  ; Parses a list of arguments
  ;  min - minimal number of arguments
  ;  max - maximal number of arguments. If max<0, then the number of 
  ; arguments is not limited from above
  ; Result:
  ;  (funcs  new-path  index-required)
  ;  funcs = (func func ...)  - functions which evaluate arguments    
  (define (sxml:parse-arguments min max path ns-binding)    
    (let ((path (sxml:parse-assert "(" path)))
      (cond
	((not path) #f)
	((= max 0)
	 (and-let* ((path (sxml:parse-assert ")" path)))
		   (list '() path #f)))
	((and (= min 0) (sxml:parse-check ")" path))
	 (list '() (sxml:parse-assert ")" path) #f))
	(else (let back ((n 1)
			 (funcs '())
			 (path path)
			 (index-required #f))
		(and-let* ((lst (sxml:parse-expr path ns-binding)))
			  (let ((func (car lst))
				(path (cadr lst))
				(bool (or index-required (caddr lst))))
			    (cond
			      ((= n max)
			       (and-let* ((path (sxml:parse-assert ")" path)))
					 (list (reverse (cons func funcs))
					       path
					       bool)))
			      ((and (>= n min) (sxml:parse-check ")" path))
			       (list (reverse (cons func funcs))
				     (sxml:parse-assert ")" path)
				     bool))
			      (else (and-let* ((path (sxml:parse-assert "," path)))
					      (back (+ n 1)
						    (cons func funcs)
						    path
						    bool)))))))))))
                 
  (and-let*
   ((lst (sxml:parse-qname path)))
   (let ((name (car lst))  ; can be a pair
         (path (cadr lst)))       
     (cond
       ((equal? name "last")  ; it is a 'last' function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 0 path ns-binding)))
         (list (lambda (nodeset root-node context var-binding) 
                 (cdr context))
               (cadr lst2)
               #f)))
       ((equal? name "position")  ; it is a 'position' function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 0 path ns-binding)))
         (list (lambda (nodeset root-node context var-binding)
                 (car context))
               (cadr lst2)
               #f)))
       ((equal? name "count")  ; it is a 'count' function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2))
               (path (cadr lst2))
               (index-required (caddr lst2)))
           (list (lambda (nodeset root-node context var-binding)
                   (let ((res (func nodeset root-node context var-binding)))
                     (cond
                       ((nodeset? res) (length res))
                       (else
                        (sxml:xpointer-runtime-error
                         "'count' function - an argument is not a nodeset. "
                         "Returning zero")
                        0))))
                 path
                 index-required))))
       ((equal? name "id")  ; it is an 'id' function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2))
               (path (cadr lst2)))
           (list
            (lambda (nodeset root-node context var-binding)
              (let* ((id-nset ((sxml:child (ntype?? 'id-index))
                               ((sxml:child (ntype?? '@@)) root-node))))
                (if
                 (null? id-nset)  ; no id-index
                 '()  ; ID function returns an empty nodeset
                 ((sxml:id (cdar id-nset))
                  (func nodeset root-node context var-binding)))))
            path
            #t))))
       ((equal? name "local-name")  ; it is a LOCAL-NAME function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 1 path ns-binding)))
         (cons
          (if
           (null? (car lst2))  ; no argument
           (lambda (nodeset root-node context var-binding)
             (cond
               ((null? nodeset) "")
               ((not (pair? (car nodeset))) "")  ; no name
               (else
                (let ((name (symbol->string (caar nodeset))))
                  (cond
                    ((string-rindex name #\:)
                     => (lambda (pos)
                          (substring name (+ pos 1) (string-length name))))
                    (else  ; a NCName
                     name))))))
           (let ((func (caar lst2)))
             (lambda (nodeset root-node context var-binding)
               (let ((obj (func nodeset root-node context var-binding)))
                 (cond
                   ((null? obj) "")  ; an empty nodeset
                   ((not (nodeset? obj))
                    (sxml:xpointer-runtime-error
                     "NAME function - an argument is not a nodeset. "
                     "Returning an empty string")
                     "")
                   ((not (pair? (car obj))) "")  ; no name
                   (else
                    (let ((name (symbol->string (caar obj))))
                      (cond
                        ((string-rindex name #\:)
                         => (lambda (pos)
                              (substring
                               name (+ pos 1) (string-length name))))
                        (else  ; a NCName
                         name)))))))))
          (cdr lst2))))
       ((equal? name "namespace-uri")  ; it is a NAMESPACE-URI function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 1 path ns-binding)))
         (cons
          (if
           (null? (car lst2))  ; no argument
           (lambda (nodeset root-node context var-binding)
             (cond
               ((null? nodeset) "")
               ((not (pair? (car nodeset))) "")  ; no name
               (else
                (let ((name (symbol->string (caar nodeset))))
                  (cond
                    ((string-rindex name #\:)
                     => (lambda (pos)
                          (substring name 0 pos)))
                    (else ""))))))  ; a NCName
           (let ((func (caar lst2)))
             (lambda (nodeset root-node context var-binding)
               (let ((obj (func nodeset root-node context var-binding)))
                 (cond
                   ((null? obj) "")  ; an empty nodeset
                   ((not (nodeset? obj))
                    (sxml:xpointer-runtime-error
                     "NAME function - an argument is not a nodeset. "
                     "Returning an empty string")
                     "")
                   ((not (pair? (car obj))) "")  ; no name
                   (else
                    (let ((name (symbol->string (caar obj))))
                      (cond
                        ((string-rindex name #\:)
                         => (lambda (pos)
                              (substring name 0 pos)))
                        (else ""))))))))) ; a NCName
          (cdr lst2))))
       ((equal? name "name")  ; it is a NAME function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 1 path ns-binding)))
         (cons
          (if
           (null? (car lst2))  ; no argument
           (lambda (nodeset root-node context var-binding)
             (cond
               ((null? nodeset) "")
               ((not (pair? (car nodeset))) "")  ; no name
               (else
                (symbol->string (caar nodeset)))))
           (let ((func (caar lst2)))
             (lambda (nodeset root-node context var-binding)
               (let ((obj (func nodeset root-node context var-binding)))
                 (cond
                   ((null? obj) "")  ; an empty nodeset
                   ((not (nodeset? obj))
                    (sxml:xpointer-runtime-error
                     "NAME function - an argument is not a nodeset. "
                     "Returning an empty string")
                     "")
                   ((not (pair? (car obj))) "")  ; no name
                   (else
                    (symbol->string (caar obj))))))))
          (cdr lst2))))                                     
       ((equal? name "string")  ; it is a 'string' function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 1 path ns-binding)))
         (let((funcs (car lst2))
              (path (cadr lst2))
              (index-required (caddr lst2)))
           (if (null? funcs)
               (list (lambda (nodeset root-node context var-binding)
                       (sxml:string nodeset))
                     path #f)
               (list (lambda (nodeset root-node context var-binding)
                       (sxml:string 
                        ((car funcs) 
                         nodeset root-node context var-binding)))
                     path
                     index-required)))))
       ((equal? name "concat")  ; it is a CONCAT function
        (and-let*
         ((lst2 (sxml:parse-arguments 2 -1 path ns-binding)))
         (let ((funcs (car lst2)))
           (cons
            (lambda (nodeset root-node context var-binding)
              (apply
               string-append
               (map
                (lambda (f)
                  (sxml:string (f nodeset root-node context var-binding)))
                funcs)))
            (cdr lst2)))))
       ((equal? name "starts-with")  ; it is a STARTS-WITH function
        (and-let*
         ((lst2 (sxml:parse-arguments 2 2 path ns-binding)))
         (let ((func1 (caar lst2))
               (func2 (cadar lst2)))
           (cons
            (lambda (nodeset root-node context var-binding)
              (let ((str1 (sxml:string
                           (func1 nodeset root-node context var-binding)))
                    (str2 (sxml:string
                           (func2 nodeset root-node context var-binding))))
                (string-prefix? str2 str1)))
            (cdr lst2)))))       
       ((equal? name "contains")  ; it is a 'contains' function
        (and-let*
         ((lst2 (sxml:parse-arguments 2 2 path ns-binding)))
         (let ((func1 (caar lst2))
               (func2 (cadar lst2))
               (path (cadr lst2))
               (index-required (caddr lst2)))
           (list
            (lambda (nodeset root-node context var-binding)
              (let ((str1 (sxml:string
                           (func1 nodeset root-node context var-binding)))
                    (str2 (sxml:string
                           (func2 nodeset root-node context var-binding))))
                (if (substring? str2 str1) #t #f)))  ; must return a boolean
            path
            index-required))))
       ((equal? name "substring-before")  ; it is a SUBSTRING-BEFORE function
        (and-let*
         ((lst2 (sxml:parse-arguments 2 2 path ns-binding)))
         (let ((func1 (caar lst2))
               (func2 (cadar lst2)))
           (cons
            (lambda (nodeset root-node context var-binding)
              (let* ((str1 (sxml:string
                            (func1 nodeset root-node context var-binding)))
                     (str2 (sxml:string
                            (func2 nodeset root-node context var-binding)))
                     (pos (substring? str2 str1)))
                (if (not pos)  ; STR1 doesn't contain STR2
                    ""
                    (substring str1 0 pos))))
            (cdr lst2)))))
       ((equal? name "substring-after")  ; it is a SUBSTRING-AFTER function
        (and-let*
         ((lst2 (sxml:parse-arguments 2 2 path ns-binding)))
         (let ((func1 (caar lst2))
               (func2 (cadar lst2)))
           (cons
            (lambda (nodeset root-node context var-binding)
              (let* ((str1 (sxml:string
                            (func1 nodeset root-node context var-binding)))
                     (str2 (sxml:string
                            (func2 nodeset root-node context var-binding)))
                     (pos (substring? str2 str1)))
                (if
                 (not pos)  ; STR1 doesn't contain STR2
                 ""
                 (substring
                  str1 (+ pos (string-length str2)) (string-length str1)))))
            (cdr lst2)))))
       ((equal? name "substring")  ; it is a SUBSTRING function
        (and-let*
         ((lst2 (sxml:parse-arguments 2 3 path ns-binding)))
         (let ((func1 (caar lst2))
               (func2 (cadar lst2)))
           (if
            (= (length (car lst2)) 2)  ; no third argument
            (cons
             (lambda (nodeset root-node context var-binding)
               (let ((str (sxml:string
                            (func1 nodeset root-node context var-binding)))
                     (num1 (sxml:number
                             (func2 nodeset root-node context var-binding))))
                 (let ((len (string-length str))
                       (start (- (inexact->exact (round num1)) 1)))
                   (if
                    (> start len)
                    ""
                    (substring
                     str
                     (if (< start 0) 0 start)
                     len)))))
             (cdr lst2))
            (let ((func3 (caddar lst2)))
              (cons
               (lambda (nodeset root-node context var-binding)
                 (let ((str (sxml:string
                             (func1 nodeset root-node context var-binding)))
                       (num1 (sxml:number
                              (func2 nodeset root-node context var-binding)))
                       (num2 (sxml:number
                              (func3 nodeset root-node context var-binding))))
                 (let* ((len (string-length str))
                        (start (- (inexact->exact (round num1)) 1))
                        (fin (+ start (inexact->exact (round num2)))))
                   (if
                    (or (> start len) (< fin 0) (< fin start))
                    ""
                    (substring
                     str
                     (if (< start 0) 0 start)
                     (if (> fin len) len fin))))))
             (cdr lst2)))))))                                                 
       ((equal? name "string-length")  ; it is a STRING-LENGTH function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 1 path ns-binding)))
         (cons
          (if
           (null? (car lst2))  ; no argument
           (lambda (nodeset root-node context var-binding)
             (string-length (sxml:string nodeset)))
           (let ((func (caar lst2)))
             (lambda (nodeset root-node context var-binding)
               (string-length
                (sxml:string
                 (func nodeset root-node context var-binding))))))
          (cdr lst2))))
       ((equal? name "normalize-space")  ; it is a NORMALIZE-SPACE function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 1 path ns-binding)))
         (cons
          (if
           (null? (car lst2))  ; no argument
           (lambda (nodeset root-node context var-binding)
             (let rpt ((src (string-split
                             (sxml:string nodeset) sxml:whitespace))
                       (res '()))
               (cond
                 ((null? src)
                  (apply string-append (reverse res)))
                 ((= (string-length (car src)) 0)  ; empty string
                  (rpt (cdr src) res))
                 ((null? res)
                  (rpt (cdr src) (cons (car src) res)))
                 (else
                  (rpt (cdr src) (cons (car src) (cons " " res)))))))
           (let ((func (caar lst2)))
             (lambda (nodeset root-node context var-binding)
               (let rpt ((src (string-split
                               (sxml:string
                                (func nodeset root-node context var-binding))
                               sxml:whitespace))
                         (res '()))
                 (cond
                   ((null? src)
                    (apply string-append (reverse res)))
                   ((= (string-length (car src)) 0)  ; empty string
                    (rpt (cdr src) res))
                   ((null? res)
                    (rpt (cdr src) (cons (car src) res)))
                   (else
                    (rpt (cdr src) (cons (car src) (cons " " res)))))))))
          (cdr lst2))))
       ((equal? name "translate")  ; it is a TRANSLATE function
        (and-let*
         ((lst2 (sxml:parse-arguments 3 3 path ns-binding)))
         (let ((func1 (caar lst2))
               (func2 (cadar lst2))
               (func3 (caddar lst2)))
           (cons
            (lambda (nodeset root-node context var-binding)
              (let ((str1 (sxml:string
                           (func1 nodeset root-node context var-binding)))
                    (str2 (sxml:string
                           (func2 nodeset root-node context var-binding)))
                    (str3 (sxml:string
                           (func3 nodeset root-node context var-binding))))
                (let ((alist
                       (let while ((lst2 (string->list str2))
                                   (lst3 (string->list str3))
                                   (alist '()))
                         (cond
                           ((null? lst2) (reverse alist))
                           ((null? lst3)
                            (append
                             (reverse alist)
                             (map
                              (lambda (ch) (cons ch #f))
                              lst2)))
                           (else
                            (while
                             (cdr lst2)
                             (cdr lst3)
                             (cons (cons (car lst2) (car lst3)) alist)))))))
                  (let rpt ((lst1 (string->list str1))
                            (res '()))
                    (cond
                      ((null? lst1) (list->string (reverse res)))
                      ((assoc (car lst1) alist)
                       => (lambda (pair)
                            (if (cdr pair)
                                (rpt (cdr lst1) (cons (cdr pair) res))
                                (rpt (cdr lst1) res))))
                      (else
                       (rpt (cdr lst1) (cons (car lst1) res))))))))
            (cdr lst2)))))                           
       ((equal? name "boolean")  ; it is a 'boolean' function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2))
               (path (cadr lst2))
               (index-required (caddr lst2)))
           (list (lambda (nodeset root-node context var-binding)
                   (sxml:boolean 
                    (func nodeset root-node context var-binding)))
                 path
                 index-required))))
       ((equal? name "not")  ; it is a 'not' function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2))
               (path (cadr lst2))
               (index-required (caddr lst2)))
           (list (lambda (nodeset root-node context var-binding)
                   (not (sxml:boolean 
                         (func nodeset root-node context var-binding))))
                 path
                 index-required))))
       ((equal? name "true")  ; it is a 'true' function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 0 path ns-binding)))
         (list (lambda (nodeset root-node context var-binding) #t)
               (cadr lst2)
               #f)))
       ((equal? name "false")  ; it is a 'false' function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 0 path ns-binding)))
         (list (lambda (nodeset root-node context var-binding) #f)
               (cadr lst2)
               #f)))
       ((equal? name "lang")  ; it is a LANG function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2)))
           (cons
            (lambda (nodeset root-node context var-binding)
              (if
               (null? nodeset)
               #f
               (let ((arg (sxml:string
                           (func nodeset root-node context var-binding)))
                     (context-node (car nodeset)))
                 (let rpt ((pairs
                            (map
                             (lambda (node) (cons node #f))
                             root-node)))
                   (if
                    (null? pairs)  ; context node not found
                    #f
                    (let* ((lng
                            ((sxml:child (ntype?? '*text*))
                             ((sxml:attribute (ntype?? 'xml:lang))
                              (caar pairs))))
                           (lng (if (null? lng) (cdar pairs) (car lng))))
                      (if
                       (eq? context-node (caar pairs)) ; context node found
                       (and
                        lng
                        (or (string-ci=? arg lng)
                            (string-prefix-ci? (string-append arg "-") lng)))
                       (rpt
                        (append
                         (map
                          (lambda (node) (cons node lng))
                          ((sxml:attribute (ntype?? '*)) (caar pairs)))
                         (map
                          (lambda (node) (cons node lng))
                          ((sxml:child sxml:node?) (caar pairs)))
                         (cdr pairs))))))))))
              (cdr lst2)))))
        ((equal? name "number")  ; it is a 'number' function
        (and-let*
         ((lst2 (sxml:parse-arguments 0 1 path ns-binding)))
         (let ((funcs (car lst2))
               (path (cadr lst2))
               (index-required (caddr lst2)))
           (if (null? funcs)
               (list
                (lambda (nodeset root-node context var-binding)
                  (sxml:number nodeset))
                path
                #f)
               (list                                                
                (lambda (nodeset root-node context var-binding)
                  (sxml:number 
                   ((car funcs) nodeset root-node context
                    var-binding)))
                path
                index-required)))))
       ((equal? name "sum")  ; it is a SUM function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2)))
           (cons          
            (lambda (nodeset root-node context var-binding)
              (let ((res (func nodeset root-node context var-binding)))
                (cond
                  ((nodeset? res)
                   (apply
                    +
                    (map
                     (lambda (node)
                       (sxml:number (sxml:string-value node)))
                     res)))
                  (else
                   (sxml:xpointer-runtime-error
                    "SUM function - an argument is not a nodeset. "
                    "Returning zero")
                   0))))
            (cdr lst2)))))       
       ((equal? name "floor")  ; it is a 'floor' function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2))
               (path (cadr lst2))
               (index-required (caddr lst2)))
           (list (lambda (nodeset root-node context var-binding)
                   (inexact->exact
                    (floor (sxml:number 
                            (func nodeset root-node context var-binding)))))
                 path
                 index-required))))
       ((equal? name "ceiling")  ; it is a 'ceiling' function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2))
               (path (cadr lst2))
               (index-required (caddr lst2)))
           (list (lambda (nodeset root-node context var-binding)
                   (inexact->exact
                    (ceiling (sxml:number
                              (func nodeset root-node context var-binding)))))
                 path
                 index-required))))
       ((equal? name "round")  ; it is a 'round' function
        (and-let*
         ((lst2 (sxml:parse-arguments 1 1 path ns-binding)))
         (let ((func (caar lst2))
               (path (cadr lst2))
               (index-required (caddr lst2)))
           (list (lambda (nodeset root-node context var-binding)
                   (inexact->exact
                    (round (sxml:number
                            (func nodeset root-node context var-binding)))))
                 path
                 index-required))))
       (else       ; unknown function
        (and-let*
         ((lst (sxml:parse-arguments 0 -1 path ns-binding)))
         (let ((path (cadr lst)))                           
           (sxml:xpointer-parse-warning
            "function not implemented - "
            (if (pair? name)
                (string-append (car name) ":" (cdr name))
                name))
           (list (lambda (nodeset root-node context var-binding) '())
                 path
                 #f))))))))
     
; Parses a PrimaryExpr production ([15] in XPath specification)
; [15]    PrimaryExpr    ::=    VariableReference  
;                               | '(' Expr ')'  
;                               | Literal  
;                               | Number  
;                               | FunctionCall 
; [29]    Literal    ::=    '"' [^"]* '"'  
;                           | "'" [^']* "'"  
; [30]    Number    ::=    Digits ('.' Digits?)?  
;                          | '.' Digits  
; [31]    Digits    ::=    [0-9]+ 
(define (sxml:parse-primary-expr path ns-binding)
  (cond
    ((sxml:parse-check "$" path)  ; a VariableReference
     (sxml:parse-variable-reference path ns-binding))
    ((sxml:parse-check "(" path)  ; an '(' Expr ')'
     (and-let* ((lst (sxml:parse-expr (sxml:parse-assert "(" path) ns-binding))
	        (path (sxml:parse-assert ")" (cadr lst))))
	 (let ((func (car lst))
	       (index-required (caddr lst)))
	     (list func path index-required))))
    ((or (sxml:parse-check "\"" path) (sxml:parse-check "'" path))  ; a Literal
     (and-let* ((lst (sxml:parse-literal path)))
	 (list
	   (lambda (nodeset root-node context var-binding) (car lst))
	   (cadr lst)
	   #f)))
    ((let ((p (sxml:skip-ws path)))
       (cond ((null? p) #f)
	     ((char=? (car p) #\.) #t)
	     ((and (char>=? (car p) #\0) (char<=? (car p) #\9)) #t)
	     (else #f)))   ; a Number
     (and-let* ((lst (sxml:parse-number path)))
	 (list
	   (lambda (nodeset root-node context var-binding) (car lst))
	   (cadr lst)
	   #f)))
    (else   ; a Function call
      (sxml:parse-function-call path ns-binding))))


; Parses a FilterExpr production ([20] in XPath specification)
; [20]    FilterExpr    ::=    PrimaryExpr  
;                              | FilterExpr Predicate 
(define (sxml:parse-filter-expr path ns-binding)
  (and-let* ((lst (sxml:parse-primary-expr path ns-binding)))
      (let ((prim (car lst)))
	(let loop ((preds '())
		   (path (cadr lst))
		   (index-required (caddr lst)))
	  (if (sxml:parse-check "[" path)
	    (and-let* ((lst (sxml:parse-predicate path ns-binding)))
		(loop (cons (car lst) preds)
		      (cadr lst)
		      (or index-required (caddr lst))))
	    (if (null? preds) ; No more predicates
	      (list prim path index-required)
	      (list
		(lambda (nodeset root-node context var-binding)
		  (let ((nodeset 
			  (prim nodeset root-node context var-binding)))
		    (sxml:xpath-nodeset-filter 
		      preds 
		      (cond
			((nodeset? nodeset) nodeset)
			(else 
			  (sxml:xpointer-runtime-error 
			    "expected - nodeset instead of " nodeset 
			    ". Converting to an empty nodeset")
			  '()))
		      root-node var-binding)))
		path
		index-required)))))))

; Parses a PathExpr production ([19] in XPath specification)
; [19]    PathExpr    ::=    LocationPath  
;                            | FilterExpr  
;                            | FilterExpr '/' RelativeLocationPath  
;                            | FilterExpr '//' RelativeLocationPath
(define (sxml:parse-path-expr path ns-binding)
  
  (define (filter-expr? path)
    (let ((path (sxml:skip-ws path)))
      (cond
        ((null? path) #f)
        ((member 
          (car path) 
          '(#\$ #\( #\" #\' #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #t)
        ((char=? (car path) #\.)
         (cond((null? (cdr path)) #f)
              ((member (cadr path)
                       '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) #t)
              (else #f)))
        ((member 
          (car path)
          '(#\) #\< #\> #\[ #\] #\/ #\+ #\* #\, #\= #\| #\! #\@ #\-)) #f)
        (else (let ((lst (sxml:parse-ncname path)))
		(cond
		  ((not lst) #f)
		  ((sxml:parse-check "::" (cadr lst)) #f)
		  (else (and-let* ((lst (sxml:parse-name path)))
				  (let ((name (car lst))
					(new-path (sxml:skip-ws (cadr lst))))
				    (cond
				      ((string=? name "range-to") #f)
				      ((string=? name "comment") #f)
				      ((string=? name "text") #f)
				      ((string=? name "processing-instruction") #f)
				      ((string=? name "node") #f)
				      ((string=? name "point") #f)
				      ((string=? name "range") #f)
				      ((null? new-path) #f)
				      ((char=? (car new-path) #\() #t)
				      (else #f))))))))))) 
  (if (not (filter-expr? path))
    (sxml:parse-location-path path ns-binding)
    (and-let* ((lst (sxml:parse-filter-expr path ns-binding)))
	      (let ((f-ex (car lst))
		    (path (cadr lst))
		    (index-required (caddr lst)))
		(cond
		  ((sxml:parse-check "//" path)
		   (and-let* ((lst2 (sxml:parse-relative-location-path 
				(sxml:parse-assert "//" path) ns-binding)))
		       (let ((f-rel (car lst2))
			    (path (cadr lst2))
			    (bool (or index-required (caddr lst2))))
			 (list
			   (lambda (nodeset root-node context var-binding)
			     (let ((nset (f-ex nodeset root-node context
					      var-binding)))
			       (let ((nset 
				      (cond
					((nodeset? nset) nset)
					(else 
					  (sxml:xpointer-runtime-error 
					    "expected - nodeset instead of " nset 
					    ". Converting to an empty nodeset")
					  '()))))
				 (let ((nset ((sxml:descendant-or-self sxml:node?) 
					     nset)))
				   (f-rel nset root-node context
					  var-binding)))))
			   path
			   bool))))
		  ((sxml:parse-check "/" path)
		   (and-let* ((lst2 (sxml:parse-relative-location-path 
				      (sxml:parse-assert "/" path) ns-binding)))
			     (let ((f-rel (car lst2))
				   (path (cadr lst2))
				   (bool (or index-required (caddr lst2))))
			       (list
				(lambda (nodeset root-node context var-binding)
				   (let ((nset (f-ex nodeset root-node context
						     var-binding)))
				     (let ((nset 
					     (cond
					       ((nodeset? nset) nset)
					       (else 
						 (sxml:xpointer-runtime-error 
						   "expected - nodeset instead of "
						   nset 
					   ". Converting to an empty nodeset")
						 '()))))                       
				       (f-rel
                                         nset root-node context var-binding))))
				 path
				 bool))))
		  (else lst))))))
                   
; Parses a UnionExpr production ([18] in XPath specification)
; [18]    UnionExpr    ::=    PathExpr  
;                             | UnionExpr '|' PathExpr
(define (sxml:parse-union-expr path ns-binding)
  (let loop ((funcs '())
	     (path path)
	     (index-required #f))
    (and-let* ((lst (sxml:parse-path-expr path ns-binding)))
	      (let ((func (car lst))
		    (path (cadr lst))
		    (id-ind? (or index-required (caddr lst))))
		(let ((new-path (sxml:parse-check "|" path)))
		  (if (not new-path)  ; no more PathExprs
		    (if (null? funcs)  ; only one PathExpr
		      (list func path id-ind?)
		      (list
			(lambda (nodeset root-node context var-binding)
			  (let rpt ((res '())
				    (fs (reverse (cons func funcs))))
			    (if (null? fs)
			      res
			      (let ((nset ((car fs) nodeset root-node context
						    var-binding)))
				(rpt 
				  (append 
				    res
				    (cond
				      ((not (nodeset? nset))
				       (sxml:xpointer-runtime-error 
					 "expected - nodeset instead of "
					 nset ". Ignoring")
				       '())
				      (else nset)))
				  (cdr fs))))))
			path
			id-ind?))
		    (loop (cons func funcs) new-path id-ind?)))))))
 
; Parses a UnaryExpr production ([27] in XPath specification)
; [27]    UnaryExpr    ::=    UnionExpr  
;                             | '-' UnaryExpr 
; Note that the grammar allows multiple unary minuses
(define (sxml:parse-unary-expr path ns-binding)
  (if (not (sxml:parse-check "-" path))
    (sxml:parse-union-expr path ns-binding)
    (let loop ((minu #f) (path path))
      (let ((new-path (sxml:parse-check "-" path)))
	(if (not new-path)  ; minuses are over
	  (and-let* ((lst (sxml:parse-union-expr path ns-binding)))
		    (let ((func (car lst))
			  (path2 (cadr lst))
			  (index-required (caddr lst)))
		      (list
			(lambda (nodeset root-node context var-binding)
			  (if minu
			    (- 0 (sxml:number 
				   (func nodeset root-node context
					 var-binding)))
			    (sxml:number 
			      (func nodeset root-node context
				    var-binding))))
			path2
			index-required)))
	  (loop (not minu) new-path))))))

; A helper for arithmetic parsers 
;   sxml:parse-additive-expr and sxml:parse-multiplicative-expr 
(define (sxml:no-more-exprs funcs func path id-ind? opers)
  (if (null? funcs) ; only one Expr
    (list func path id-ind?)
    (let ((flst (reverse (cons func funcs))))
      (list (lambda (nodeset root-node context var-binding)
	  (let rpt ((res (sxml:number
			   ((car flst) nodeset root-node context
				       var-binding)))
		    (fs (cdr flst))
		    (ops (reverse opers)))
	    (if (null? fs)
	      res
	      (rpt ((car ops)
		    res
		    (sxml:number 
		      ((car fs) nodeset root-node context
				var-binding)))
		   (cdr fs)
		   (cdr ops)))))
	path
	id-ind?))))

; Parses a MultiplicativeExpr production ([26],[34] in XPath specification)
; [26] MultiplicativeExpr  ::=  UnaryExpr  
;                               | MultiplicativeExpr MultiplyOperator UnaryExpr
;                               | MultiplicativeExpr 'div' UnaryExpr  
;                               | MultiplicativeExpr 'mod' UnaryExpr 
; [34] MultiplyOperator  ::=  '*'
(define (sxml:parse-multiplicative-expr path ns-binding)
  (let loop ((funcs '())
             (opers '())
             (path path)
             (index-required #f))
    (and-let* ((lst (sxml:parse-unary-expr path ns-binding)))
         (let ((func (car lst))
              (path (cadr lst))
              (id-ind? (or index-required (caddr lst))))
           (cond
             ((sxml:parse-check "*" path)
              (loop (cons func funcs) (cons * opers)
                    (sxml:parse-assert "*" path) id-ind?))
             ((sxml:parse-check "div" path sxml:delimiter)
              (loop (cons func funcs) (cons / opers)
                    (sxml:parse-assert "div" path) id-ind?))
             ((sxml:parse-check "mod" path sxml:delimiter)
              (loop (cons func funcs) (cons remainder opers)
                    (sxml:parse-assert "mod" path) id-ind?))             
             (else  ; no more UnaryExprs
	       (sxml:no-more-exprs funcs func path id-ind? opers)
	      ))))))

; Parses a AdditiveExpr production ([25] in XPath specification)
; [25]    AdditiveExpr    ::=    MultiplicativeExpr  
;                                | AdditiveExpr '+' MultiplicativeExpr  
;                                | AdditiveExpr '-' MultiplicativeExpr 
(define (sxml:parse-additive-expr path ns-binding)
  (let loop ((funcs '())
             (opers '())
             (path path)
             (index-required #f))
    (and-let* ((lst (sxml:parse-multiplicative-expr path ns-binding)))
         (let ((func (car lst))
              (path (cadr lst))
              (id-ind? (or index-required (caddr lst))))
           (cond
             ((sxml:parse-check "+" path)
              (loop (cons func funcs) (cons + opers)
                    (sxml:parse-assert "+" path) id-ind?))
             ((sxml:parse-check "-" path)
              (loop (cons func funcs) (cons - opers)
                    (sxml:parse-assert "-" path) id-ind?))
             (else  ; no more MultiplicativeExprs
                (sxml:no-more-exprs funcs func path id-ind? opers)
	      ))))))

; Parses a RelationalExpr production ([24] in XPath specification)
; [24]    RelationalExpr    ::=    AdditiveExpr  
;                                  | RelationalExpr '<' AdditiveExpr  
;                                  | RelationalExpr '>' AdditiveExpr  
;                                  | RelationalExpr '<=' AdditiveExpr  
;                                  | RelationalExpr '>=' AdditiveExpr 
(define (sxml:parse-relational-expr path ns-binding)
  (let loop ((funcs '())
             (opers '())
             (path path)
             (index-required #f))
    (let ((lst (sxml:parse-additive-expr path ns-binding)))
      (if (not lst)
         #f
         (let ((func (car lst))
              (path (cadr lst))
              (bool (or index-required (caddr lst))))
           (cond
             ((sxml:parse-check "<=" path)
              (loop (cons func funcs) (cons (sxml:relational-cmp <=) opers)
                    (sxml:parse-assert "<=" path) bool))
             ((sxml:parse-check ">=" path)
              (loop (cons func funcs) (cons (sxml:relational-cmp >=) opers)
                    (sxml:parse-assert ">=" path) bool))             
             ((sxml:parse-check "<" path)
              (loop (cons func funcs) (cons (sxml:relational-cmp <) opers)
                    (sxml:parse-assert "<" path) bool))
             ((sxml:parse-check ">" path)
              (loop (cons func funcs) (cons (sxml:relational-cmp >) opers)
                    (sxml:parse-assert ">" path) bool))
             (else  ; no more AdditiveExprs
              (if (null? funcs) ; only one AdditiveExpr  ; sxml:no-more-helper ?
                 (list func path bool)
                 (let ((flst (reverse (cons func funcs))))
                   (list                  
                    (lambda (nodeset root-node context var-binding)
                      (let rpt ((res ((car flst) nodeset root-node context
                                      var-binding))
                                (fs (cdr flst))
                                (ops (reverse opers)))
                        (if (null? fs) 
                           res
                           (rpt ((car ops) 
                                 res 
                                 ((car fs) nodeset root-node context
                                           var-binding))
                                (cdr fs)
                                (cdr ops)))))
                    path
                    bool)))
	      )))))))


; Parses an EqualityExpr production ([23] in XPath specification)
; [23]    EqualityExpr    ::=    RelationalExpr  
;                                | EqualityExpr '=' RelationalExpr  
;                                | EqualityExpr '!=' RelationalExpr 
(define (sxml:parse-equality-expr path ns-binding)
  (let loop ((funcs '())
	     (opers '())
	     (path path)
	     (index-required #f))
    (and-let* ((lst (sxml:parse-relational-expr path ns-binding)))
	      (let ((func (car lst))
		    (path (cadr lst))
		    (id-ind? (or index-required (caddr lst))))
		(cond
		  ((sxml:parse-check "=" path)
		   (loop (cons func funcs) (cons sxml:equal? opers)
			 (sxml:parse-assert "=" path) id-ind?))
		  ((sxml:parse-check "!=" path)
		   (loop (cons func funcs) (cons sxml:not-equal? opers)
			 (sxml:parse-assert "!=" path) id-ind?))
		  (else  ; no more RelationalExprs
		    (if (null? funcs) ; only one RelationalExpr
		      (list func path id-ind?)
		      (let ((flst (reverse (cons func funcs))))
			(list                  
			  (lambda (nodeset root-node context var-binding)
			    (let rpt ((res ((car flst) nodeset root-node context
						       var-binding))
				      (fs (cdr flst))
				      (ops (reverse opers)))
			      (if (null? fs)
				res
				(rpt ((car ops) 
                                      res 
                                      ((car fs) nodeset root-node context
                                       var-binding))
				     (cdr fs)
				     (cdr ops)))))
			  path
			  id-ind?)))
		    ))))))

; Parses an AndExpr production ([22] in XPath specification)
; [22]    AndExpr    ::=    EqualityExpr  
;                           | AndExpr 'and' EqualityExpr 
; Note that according to 3.4 in XPath specification, the right operand is not
; evaluated if the left operand evaluates to false
(define (sxml:parse-and-expr path ns-binding)
  (let loop ((funcs '())
             (path path)
             (index-required #f))
    (and-let* ((lst (sxml:parse-equality-expr path ns-binding)))
         (let ((func (car lst))
               (path (cadr lst))
               (id-ind? (or index-required (caddr lst))))
           (let ((new-path (sxml:parse-check "and" path sxml:delimiter)))
             (if (not new-path)  ; no more EqualityExprs
                (if (null? funcs)  ; only one EqualityExpr
                   (list func path id-ind?)
                   (list
                    (lambda (nodeset root-node context var-binding)
                      (let rpt ((fs (reverse (cons func funcs))))
                        (cond
                          ((null? fs) #t)
                          ((not (sxml:boolean
                                 ((car fs) nodeset root-node context
                                  var-binding))) #f)
                          (else (rpt (cdr fs))))))
                    path
                    id-ind?))
                (loop (cons func funcs) new-path id-ind?)))))))


; Parses an Expr production ([14],[21] in XPath specification)
; [14]    Expr    ::=    OrExpr 
; [21]    OrExpr    ::=    AndExpr  
;                          | OrExpr 'or' AndExpr
; Note that according to 3.4 in XPath specification, the right operand is not
; evaluated if the left operand evaluates to true
(define (sxml:parse-expr path ns-binding)
  (let loop ((funcs '())
	     (path path)
	     (index-required #f))
    (and-let* ((lst (sxml:parse-and-expr path ns-binding)))
	      (let ((func (car lst))
		    (path (cadr lst))
		    (id-ind? (or index-required (caddr lst))))
		(let ((new-path (sxml:parse-check "or" path sxml:delimiter)))
		  (if (not new-path)  ; no more AndExprs
		    (if (null? funcs)  ; only one AndExpr
		      (list func path id-ind?)
		      (list
			(lambda (nodeset root-node context var-binding)
			  (let rpt ((fs (reverse (cons func funcs))))
			    (cond
			      ((null? fs) #f)
			      ((sxml:boolean
				 ((car fs) nodeset root-node context
					   var-binding)) #t)
			      (else (rpt (cdr fs))))))
			path
			id-ind?))
		    (loop (cons func funcs) new-path id-ind?)))))))
                           
;------------------------------------------------
; Functions which parse XPointer grammar
;
; The return value has the same signature for all these functions (and it
; coincides with the signature from the previous section)
; (list (lambda (nodeset root-node context var-binding) ...)
;       path 
;       index-required )
; or #f
; #f signals of a parse error (error message is printed as a side effect
; during parsing)
;
; (lambda (nodeset root-node context var-binding) - an SXPath-like 
; function (it transforms a nodeset into a new nodeset)
;  nodeset - a current set of nodes
;  root-node - the root of a document (a singleton nodeset)
;  context - the context of the node:  context = (cons position size)
;  position - context position (a number)
;  size - context size (a number)
;  path - an XPointer path represented as the list of chars
;  index-required - boolean value: whether an id-index was required "deeper" 
; within the grammar


; Parses an FullXPtr production ([3]-[10] in XPointer specification)
; [3]    FullXPtr    ::=    XPtrPart (S? XPtrPart)* 
; [4]    XPtrPart    ::=    'xpointer' '(' XPtrExpr ')'
;                           | 'xmlns' '(' XPtrNsDecl? ')' 
;                           | Scheme '(' SchemeSpecificExpr ')' 
; [5]    Scheme    ::=    NCName 
; [6]    SchemeSpecificExpr    ::=    StringWithBalancedParens 
; [7]    StringWithBalancedParens    ::=
;                    [^()]* ('(' StringWithBalancedParens ')' [^()]*)*
; [8]    XPtrExpr    ::=    Expr
; [9]    XPtrNsDecl    ::=    NCName S? '=' S? XPtrNsURI 
; [10]    XPtrNsURI    ::=    Char*
(define (sxml:parse-full-xptr path ns-binding)
  (let loop ((funcs '())
             (ns-binding ns-binding)
             (path path)
             (index-required #f))
    (if (null? (sxml:skip-ws path))  ; the string is over
     (if (= (length funcs) 1)
      (list (car funcs) path index-required)
      (list (lambda (nodeset root-node context var-binding)
         (let rpt ((fs (reverse funcs)))
           (if (null? fs)
              '()
              (let ((nset ((car fs) nodeset root-node context
                          var-binding)))
                (if (null? nset)
                   (rpt (cdr fs))
                   nset)))))
       path
       index-required))
     (and-let* ((lst (sxml:parse-name path))
                (name (car lst))
                (path (cadr lst)))
          (cond
            ((string=? name "xpointer")
             (and-let* ((path (sxml:parse-assert "(" path))
                        (lst2 (sxml:parse-expr path ns-binding)))
                       (let ((func (car lst2))
                             (path (cadr lst2))
                             (bool (caddr lst2)))
                         (and-let* ((path (sxml:parse-assert ")" path)))
                              (loop (cons func funcs) ns-binding
                                    path (or index-required bool))))))
            ((string=? name "xmlns")
             (and-let* ((path0 (sxml:parse-assert "(" path))
                        (lst2 (sxml:parse-ncname path0))
                        (prefix (string->symbol (car lst2)))
                        (path (sxml:parse-assert "=" (cadr lst2))))
                      (let rpt2 ((path (sxml:skip-ws path)) (uri '()))
                        (cond
                          ((null? path)
                           (sxml:parse-assert ")" path)
                           #f)
                          ((and (char=? (car path) #\)) (null? uri))
                           (sxml:xpointer-parse-error
                            "namespace URI cannot be empty"))
                          ((char=? (car path) #\))
                           (loop funcs 
                                 (cons 
                                  (cons prefix (list->string (reverse uri)))
                                  ns-binding)
                                 (cdr path)
                                 index-required))
                          (else (rpt2 (cdr path) (cons (car path) uri)))))))
            (else (and-let* ((path (sxml:parse-assert "(" path)))
                  (let rpt3 ((n 1) (path path))
                    (cond
                      ((= n 0)
                       (sxml:xpointer-parse-warning
                        "unknown xpointer schema - " name ". Ignoring")
                       (loop funcs ns-binding path index-required))
                      ((null? path)
                       (sxml:parse-assert ")" path)
                       #f)
                      ((char=? (car path) #\() (rpt3 (+ n 1) (cdr path)))
                      ((char=? (car path) #\)) (rpt3 (- n 1) (cdr path)))
                      (else (rpt3 n (cdr path))))))))))))

; Parses an ChildSeq production ([2] in XPointer specification)
; [2]    ChildSeq    ::=    Name? ('/' [1-9] [0-9]* )+ 
(define (sxml:parse-child-seq path)
       
  (define (helper path)
    (let loop ((funcs '()) (path path))
      (let ((path2 (sxml:parse-check "/" path)))
        (if (not path2)  ; no more #\/
           (if (null? (sxml:skip-ws path))  ; end of path
              (reverse funcs)
              (sxml:parse-assert "/" path))  ; this will cause an error message
           (and-let* ((lst (sxml:parse-natural path2)))
                (loop (cons 
                       (node-pos (car lst))
                       (cons (sxml:child (ntype?? '*)) funcs))
                      (cadr lst)))))))
  
  (let ((path2 (sxml:parse-check "/" path)))
    (if
     (not path2)
     (and-let*
      ((lst (sxml:parse-name path))
       (name (car lst))
       (funcs (helper (cadr lst))))
      (list
       (lambda (nodeset root-node context var-binding)
         (let* ((id-nset ((sxml:child (ntype?? 'id-index))
                          ((sxml:child (ntype?? '@@)) root-node))))
           (if
            (null? id-nset)  ; no id-index
            '()                       
            (let ((nd (sxml:lookup name (cdar id-nset))))
              (if (not nd)
                  '()
                  (let rpt ((nset (list nd))
                            (fs funcs))
                    (if (null? fs)
                        nset
                        (rpt ((car fs) nset) (cdr fs)))))))))
       '()
       #t))
     (and-let*
      ((funcs (helper path)))
      (list
       (lambda (nodeset root-node context var-binding)
         (if (nodeset? nodeset)
             (let rpt ((nodeset nodeset) (res '()))
                 (if (null? nodeset)
                     res
                     (let rpt2 ((nset (list (car nodeset))) 
                                (fs funcs))
                       (if (null? fs)
                           (rpt (cdr nodeset) (append res nset))
                           (rpt2 ((car fs) nset) (cdr fs))))))
             (let rpt ((nodeset nodeset)
                       (fs funcs))
               (if (null? fs)
                   nodeset
                   (rpt ((car fs) nodeset) (cdr fs))))))
       '()
       #f)))))
                                                                  
; Parses an XPointer production ([1] in XPointer specification)
; [1]    XPointer    ::=    Name | ChildSeq | FullXPtr 
(define (sxml:parse-xpointer path ns-binding)
  (if (sxml:parse-check "/" path)   ; #\/ is the first symbol => ChildSeq
    (sxml:parse-child-seq path)
    (and-let* ((lst (sxml:parse-name path))
	       (new-path (cadr lst)))
	      (if (sxml:parse-check "(" new-path)  ; FullXPtr production
		(sxml:parse-full-xptr path ns-binding)
		(sxml:parse-child-seq path)))))
      
;=========================================================================
; Highest level API functions

;------------------------------------------------
; 'sxml:xpath' and 'sxml:xpointer' functions
;
;  xpath-string - an XPath query (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (lambda (node) ...)  
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node) ...)  - an SXPath function
;  node - a root node of the SXML document

(define (sxml:api-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (and-let*
     ((res (parse-proc
            (string->list xpath-string)
            (if (null? ns-binding) ns-binding (car ns-binding)))))
     (let ((func (car res)))    
       (lambda (node)
         (let ((node (if (nodeset? node) node (list node))))
           (func node node (cons 1 1) '())))))))

(define sxml:xpath (sxml:api-helper sxml:parse-xpath))

(define sxml:xpointer (sxml:api-helper sxml:parse-xpointer))


;------------------------------------------------
; 'sxml:xpath+index' and 'sxml:xpointer+index' functions
; 
;  xpath-string - an XPath query (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (cons (lambda (node . id-index) ...)  
;                              index-required )
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node) ...)  - an SXPath function
;  node - a root node of the SXML document
;  index-required - a boolean value: whether an id-index is required

(define (sxml:api-index-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (and-let*
     ((res (parse-proc
            (string->list xpath-string)
            (if (null? ns-binding) ns-binding (car ns-binding)))))
     (let ((func (car res)))
       (cons
        (lambda (node)
          (let ((node (if (nodeset? node) node (list node))))
            (func node node (cons 1 1) '())))
        (caddr res))))))
     
(define sxml:xpath+index (sxml:api-index-helper sxml:parse-xpath))

(define sxml:xpointer+index (sxml:api-index-helper sxml:parse-xpointer))


;------------------------------------------------
; 'sxml:xpath+root+vars' and 'sxml:xpointer+root+vars' functions
; 
;  xpath-string - an XPath query (a string)
;  ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding = (list  (prefix . uri)
;                      (prefix . uri)
;                      ...)
;  prefix - a symbol
;  uri - a string
;
; The returned result:   (lambda (node root-node . var-binding) ...)
;                   or   #f
;  #f - signals of a parse error (error message is printed as a side effect
; during parsing)
;  (lambda (node root-node . var-binding) ...)  - an SXPath function
;  node - a node (or a node-set) of the SXML document
;  root-node - the root of the document
;  var-binding = (list  (var-name . value)
;                       (var-name . value)
;                       ...)
;  var-name - (a symbol) a name of a variable
;  value - its value. The value can have the following type: boolean, number,
; string, nodeset. NOTE: a node must be represented as a singleton nodeset

(define (sxml:api-root-vars-helper parse-proc)
  (lambda (xpath-string . ns-binding)
    (and-let*
     ((res (parse-proc
            (string->list xpath-string)
            (if (null? ns-binding) ns-binding (car ns-binding)))))
     (let ((func (car res)))
       (lambda (node . root-var-binding)
         (let ((root-node
                (if (null? root-var-binding) node (car root-var-binding))))
         (func
          (if (nodeset? node) node (list node))
          (if (nodeset? root-node) root-node (list root-node))
          (cons 1 1)
          (if (or (null? root-var-binding) (null? (cdr root-var-binding)))
              '() (cadr root-var-binding)))))))))
      
(define sxml:xpath+root+vars (sxml:api-root-vars-helper sxml:parse-xpath))

(define sxml:xpointer+root+vars (sxml:api-root-vars-helper sxml:parse-xpointer))
      
; A stub
(define sxml:xpath+root sxml:xpath+root+vars)

(define txpath sxml:xpath+root+vars)
