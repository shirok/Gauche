;		XML/HTML processing in Scheme
;		SXML expression tree transformers
;
; $ Id: SXML-tree-trans.scm,v 1.5 2001/10/03 22:39:39 oleg Exp oleg $

; The following macro runs built-in test cases -- or does not run,
; depending on which of the two lines below you commented out
;(define-macro (run-test . body) `(begin (display "\n-->Test\n") ,@body))
(define-macro (run-test . body) '(begin #f))

; Output the 'fragments'
; The fragments are a list of strings, characters,
; numbers, thunks, #f -- and other fragments.
; The function traverses the tree depth-first, writes out
; strings and characters, executes thunks, and ignores
; #f and '().
; The function returns #t if anything was written at all;
; otherwise the result is #f

(define (SRV:send-reply . fragments)
  (let loop ((fragments fragments) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
        (display (car fragments))
        (loop (cdr fragments) #t)))))



;------------------------------------------------------------------------
;	          Traversal of an SXML tree or a grove:
;			a <Node> or a <Nodeset>
;
; A <Node> and a <Nodeset> are mutually-recursive datatypes that
; underlie the SXML tree:
;	<Node> ::= (name . <Nodeset>) | "text string"
; An (ordered) set of nodes is just a list of the constituent nodes:
; 	<Nodeset> ::= (<Node> ...)
; Nodesets, and Nodes other than text strings are both lists. A
; <Nodeset> however is either an empty list, or a list whose head is
; not a symbol (an atom in general). A symbol at the head of a node is
; either an XML name (in which case it's a tag of an XML element), or
; an administrative name such as '@'.
; See SXPath.scm and SSAX.scm for more information on SXML.

; Borrowed from SXPath.scm
(define (nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

	; Apply proc to each element of lst and return the list of results.
	; if proc returns a nodeset, splice it into the result
(define (map-union proc lst)
  (if (null? lst) lst
      (let ((proc-res (proc (car lst))))
	((if (nodeset? proc-res) append cons)
	 proc-res (map-union proc (cdr lst))))))

; Post-order traversal of a tree and creation of a new tree:
;	post-order:: <tree> x <bindings> -> <new-tree>
; where
; <bindings> ::= (<binding> ...)
; <binding> ::= (<trigger-symbol> <new-bindings> . <handler>) |
;		(<trigger-symbol> . <handler>)
; <trigger-symbol> ::= XMLname | *text* | *default*
; <handler> :: <trigger-symbol> x [<tree>] -> <new-tree>
;
; The post-order function visits the nodes and nodesets post-order
; (depth-first).  For each <Node> of the form (name <Node> ...) it
; looks up an association with the given name among its <bindings>. If
; failed, post-order tries to locate a *default* binding. It's an
; error if the latter attempt fails as well.  Having found a binding,
; the post-order function first calls itself recursively for each
; child of the current node, with <new-bindings> prepended to the
; <bindings> in effect. The result of these calls is passed to the
; <handler> (along with the head of the current <Node>). To be more
; precise, the handler is _applied_ to the head of the current node
; and its processed children. The result of the handler, which should
; also be a <tree>, replaces the current <Node>. If the current <Node>
; is a text string, a special binding with a symbol *text* is looked
; up.

(define (post-order tree bindings)
  (cond
   ((nodeset? tree)
    (map (lambda (a-tree) (post-order a-tree bindings)) tree))
   ((not (pair? tree))
    (let ((trigger '*text*))
      (cond
       ((or (assq trigger bindings) (assq '*default* bindings)) =>
	(lambda (binding)
	  ((if (procedure? (cdr binding)) (cdr binding) (cddr binding))
	   trigger tree)))
       (else
	(error "Unknown binding for " trigger " and no default")))
      ))
   (else
    (let ((trigger (car tree)))
      (cond
       ((or (assq trigger bindings) (assq '*default* bindings)) =>
	(lambda (binding)
	  (apply
	   (if (procedure? (cdr binding)) (cdr binding) (cddr binding))
	   (cons trigger 
		 (post-order (cdr tree) 
			     (if (pair? (cdr binding))
				 (append (cadr binding) bindings)
				 bindings))))))
       (else
	(error "Unknown binding for " trigger " and no default"))))))
)


; Pre-Post-order traversal of a tree and creation of a new tree:
;	pre-post-order:: <tree> x <bindings> -> <new-tree>
; where
; <bindings> ::= (<binding> ...)
; <binding> ::= (<trigger-symbol> *preorder* . <handler>) |
;		(<trigger-symbol> <new-bindings> . <handler>) |
;		(<trigger-symbol> . <handler>)
; <trigger-symbol> ::= XMLname | *text* | *default*
; <handler> :: <trigger-symbol> x [<tree>] -> <new-tree>
;
; The pre-post-order function visits the nodes and nodesets pre-post-order
; (depth-first).  For each <Node> of the form (name <Node> ...) it
; looks up an association with the given 'name' among its <bindings>. If
; failed, pre-post-order tries to locate a *default* binding. It's an
; error if the latter attempt fails as well.  Having found a binding,
; the pre-post-order function first checks to see if the binding is
; of the form
;	(<trigger-symbol> *preorder* . <handler>)
; If it is, the handler is 'applied' to the current node. Otherwise,
; the pre-post-order function first calls itself recursively for each
; child of the current node, with <new-bindings> prepended to the
; <bindings> in effect. The result of these calls is passed to the
; <handler> (along with the head of the current <Node>). To be more
; precise, the handler is _applied_ to the head of the current node
; and its processed children. The result of the handler, which should
; also be a <tree>, replaces the current <Node>. If the current <Node>
; is a text string, a special binding with a symbol *text* is looked
; up.

(define (pre-post-order tree bindings)
  (cond
   ((nodeset? tree)
    (map (lambda (a-tree) (pre-post-order a-tree bindings)) tree))
   ((not (pair? tree))
    (let ((trigger '*text*))
      (cond
       ((or (assq trigger bindings) (assq '*default* bindings)) =>
	(lambda (binding)
	  ((if (procedure? (cdr binding)) (cdr binding) (cddr binding))
	   trigger tree)))
       (else
	(error "Unknown binding for " trigger " and no default")))
      ))
   (else
    (let ((trigger (car tree)))
      (cond
       ((or (assq trigger bindings) (assq '*default* bindings)) =>
	(lambda (binding)
	  (if (and (pair? (cdr binding)) (eq? '*preorder* (cadr binding)))
	      (apply (cddr binding) tree)
	      (apply
	       (if (procedure? (cdr binding)) (cdr binding) (cddr binding))
	       (cons trigger 
		     (pre-post-order (cdr tree) 
				     (if (pair? (cdr binding))
					 (append (cadr binding) bindings)
					 bindings)))))))
       (else
	(error "Unknown binding for " trigger " and no default"))))))
)

;------------------------------------------------------------------------
;			Extended tree fold
; tree = atom | (node-name tree ...)
;
; foldts fdown fup fhere seed (Leaf str) = fhere seed str
; foldts fdown fup fhere seed (Nd kids) =
;         fup seed $ foldl (foldts fdown fup fhere) (fdown seed) kids

; procedure fhere: seed -> atom -> seed
; procedure fdown: seed -> node -> seed
; procedure fup: parent-seed -> last-kid-seed -> node -> seed
; foldts returns the final seed

(define (foldts fdown fup fhere seed tree)
  (cond
   ((null? tree) seed)
   ((not (pair? tree))		; An atom
    (fhere seed tree))
   (else
    (let loop ((kid-seed (fdown seed tree)) (kids (cdr tree)))
      (if (null? kids)
	  (fup seed kid-seed tree)
	  (loop (foldts fdown fup fhere kid-seed (car kids))
		(cdr kids)))))))

;------------------------------------------------------------------------
; Traverse a forest depth-first and cut/replace ranges of nodes.
;
; The nodes that define a range don't have to have the same immediate
; parent, don't have to be on the same level, and the end node of a
; range doesn't even have to exist. A replace-range procedure removes
; nodes from the beginning node of the range up to (but not including)
; the end node of the range.  In addition, the beginning node of the
; range can be replaced by a node or a list of nodes. The range of
; nodes is cut while depth-first traversing the forest. If all
; branches of the node are cut a node is cut as well.  The procedure
; can cut several non-overlapping ranges from a forest.

;	replace-range:: BEG-PRED x END-PRED x FOREST -> FOREST
; where
;	type FOREST = (NODE ...)
;	type NODE = Atom | (Name . FOREST) | FOREST
;
; The range of nodes is specified by two predicates, beg-pred and end-pred.
;	beg-pred:: NODE -> #f | FOREST
;	end-pred:: NODE -> #f | FOREST
; The beg-pred predicate decides on the beginning of the range. The node
; for which the predicate yields non-#f marks the beginning of the range
; The non-#f value of the predicate replaces the node. The value can be a
; list of nodes. The replace-range procedure then traverses the tree and skips
; all the nodes, until the end-pred yields non-#f. The value of the end-pred
; replaces the end-range node. The new end node and its brothers will be
; re-scanned.
; The predicates are evaluated pre-order. We do not descend into a node that
; is marked as the beginning of the range.

(define (replace-range beg-pred end-pred forest)

  ; loop forest keep? new-forest
  ; forest is the forest to traverse
  ; new-forest accumulates the nodes we will keep, in the reverse
  ; order
  ; If keep? is #t, keep the curr node if atomic. If the node is not atomic,
  ; traverse its children and keep those that are not in the skip range.
  ; If keep? is #f, skip the current node if atomic. Otherwise,
  ; traverse its children. If all children are skipped, skip the node
  ; as well.

  (define (loop forest keep? new-forest)
    (if (null? forest) (values (reverse new-forest) keep?)
	(let ((node (car forest)))
	  (if keep?
	      (cond			; accumulate mode
	       ((beg-pred node) =>	; see if the node starts the skip range
		(lambda (repl-branches)	; if so, skip/replace the node
		  (loop (cdr forest) #f 
			(append (reverse repl-branches) new-forest))))
	       ((not (pair? node))	; it's an atom, keep it
		(loop (cdr forest) keep? (cons node new-forest)))
	       (else
		(let-values* 
		 ((node? (symbol? (car node)))  ; or is it a nodeset?
		  ((new-kids keep?)		; traverse its children
		   (loop (if node? (cdr node) node) #t '())))
		 (loop (cdr forest) keep?
		       (cons 
			(if node? (cons (car node) new-kids) new-kids)
			new-forest)))))
	      ; skip mode
	      (cond
	       ((end-pred node) =>	; end the skip range
		(lambda (repl-branches)	; repl-branches will be re-scanned
		  (loop (append repl-branches (cdr forest)) #t
			new-forest)))
	       ((not (pair? node))	; it's an atom, skip it
		(loop (cdr forest) keep? new-forest))
	       (else
		(let-values* 
		 ((node? (symbol? (car node)))  ; or is it a nodeset?
		  ((new-kids keep?)		; traverse its children
		   (loop (if node? (cdr node) node) #f '())))
		 (loop (cdr forest) keep?
		       (if (or keep? (pair? new-kids))
			   (cons
			    (if node? (cons (car node) new-kids) new-kids)
			    new-forest)
			   new-forest)		; if all kids are skipped
		       ))))))))			; skip the node too
  
  (let-values* (((new-forest keep?) (loop forest #t '())))
     new-forest))

; A few tests
(run-test
 (let* ((tree
	 '(root
	      (n1 (n11) "s12" (n13))
	    "s2"
	    (n2 (n21) "s22")
	    (n3 
	     (n31 (n311))
	     "s32"
	     (n33 (n331) "s332" (n333))
	     "s34")))
	(test
	 (lambda (pred-begin pred-end expected)
	   (let ((computed
		  (car (replace-range pred-begin pred-end (list tree)))))
	     (assert (equal? computed expected)))))
	)
   (pp tree)
      ; Remove one node, "s2"
   (test
    (lambda (node)
      (and (equal? node "s2") '()))
    (lambda (node) (list node))
    '(root (n1 (n11) "s12" (n13))
       (n2 (n21) "s22")
       (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34")))

      ; Replace one node, "s2" with "s2-new"
   (test 
    (lambda (node)
      (and (equal? node "s2") '("s2-new")))
    (lambda (node) (list node))
    '(root (n1 (n11) "s12" (n13))
       "s2-new"
       (n2 (n21) "s22")
       (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34")))

      ; Replace one node, "s2" with "s2-new" and its brother (n-new "s")
   (test 
    (lambda (node)
      (and (equal? node "s2") '("s2-new" (n-new "s"))))
    (lambda (node) (list node))
    '(root (n1 (n11) "s12" (n13))
       "s2-new" (n-new "s")
       (n2 (n21) "s22")
       (n3 (n31 (n311)) "s32" (n33 (n331) "s332" (n333)) "s34")))

      ; Remove everything from "s2" onward
   (test 
    (lambda (node)
      (and (equal? node "s2") '()))
    (lambda (node) #f)
    '(root (n1 (n11) "s12" (n13))))
   
      ; Remove everything from "n1" onward
   (test 
    (lambda (node)
      (and (pair? node) (eq? 'n1 (car node)) '()))
    (lambda (node) #f)
    '(root))

      ; Replace from n1 through n33
   (test 
    (lambda (node)
      (and (pair? node)
	   (eq? 'n1 (car node))
	   (list node '(n1* "s12*"))))
    (lambda (node)
      (and (pair? node)
	   (eq? 'n33 (car node))
	   (list node)))
    '(root
	 (n1 (n11) "s12" (n13))
       (n1* "s12*")
       (n3 
	(n33 (n331) "s332" (n333))
	"s34")))
   ))
