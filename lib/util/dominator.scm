;;;
;;; dominator.scm - Calculate dominators of control graph nodes
;;;
;;;   Copyright (c) 2015-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module util.dominator
  (use srfi-42)
  (export calculate-dominators))
(select-module util.dominator)

;; Based on
;; Keith D. Cooper, Timothy J. Harvey, and Ken Kennedy:
;; A Simple, Fast Dominance Algorithm,
;; Software Practice and Experience, Vol.4 pp.1-10, 2001.

;; A graph is represented in (start, upstreams, downstreams, node-comparator)
;; where
;;  start :: Node
;;  upstreams :: Node -> [Node]
;;  downstreams :: Node -> [Node]
;;  node-comparator :: comparator (with hash function)
;;
;; Returns
;;  [(node, immediate-dominator-node)]

(define (calculate-dominators start upstreams downstreams node-comparator)
  (define t:id->node (make-hash-table 'eqv?))
  (define t:upstream-ids (make-hash-table 'eqv?)) ; id -> [id]
  (define t:downstream-ids (make-hash-table 'eqv?)) ; id -> [id]
  (define t:node->id (make-hash-table node-comparator))
  (define (id->node id) (hash-table-get t:id->node id #f))
  (define (node->id node) (hash-table-get t:node->id node #f))
  (define (id->upstream-ids id) (hash-table-get t:upstream-ids id '()))
  (define (id->downstream-ids id) (hash-table-get t:downstream-ids id '()))

  (define (postorder node next-id) ; returns next id
    (if (node->id node)
      next-id
      (begin
        (hash-table-put! t:node->id node #t) ; mark visited
        (let1 next-id (fold postorder next-id (downstreams node))
          (hash-table-put! t:id->node next-id node)
          (hash-table-put! t:node->id node next-id)
          (+ next-id 1)))))

  (define (setup! node)
    (let1 id (node->id node)
      (unless (hash-table-exists? t:upstream-ids id)
        (hash-table-put! t:upstream-ids id
                         (filter-map node->id (upstreams node)))
        (hash-table-put! t:downstream-ids id
                         (filter-map node->id (downstreams node)))
        (for-each setup! (downstreams node)))))

  (define (update doms nid changed?)
    (let1 b0 (any (^n (and (vector-ref doms n) n)) (id->upstream-ids nid))
      (unless b0
        (error "calculate-dominators: something wrong with input graph"))
      (let1 new-idom (fold (^[n new-idom]
                             (if-let1 m (and (not (eq? n b0))
                                             (vector-ref doms n))
                               (intersect doms m new-idom)
                               new-idom))
                           b0 (id->upstream-ids nid))
        (if (eqv? (vector-ref doms nid) new-idom)
          changed?
          (begin (vector-set! doms nid new-idom) #t)))))

  (define (iterate! doms max-nid)
    (when (fold (^[nid changed?] (update doms nid changed?))
                #f (iota max-nid (- max-nid 1) -1))
      (iterate! doms max-nid)))

  (define (intersect doms b1 b2)
    (let loop ([finger1 b1]
               [finger2 b2])
      (cond [(= finger1 finger2) finger1]
            [(< finger1 finger2) (loop (vector-ref doms finger1) finger2)]
            [else (loop finger1 (vector-ref doms finger2))])))
                  
  (let* ([nnodes (postorder start 0)]
         [doms (make-vector nnodes #f)])
    (vector-set! doms (- nnodes 1) (- nnodes 1))
    (setup! start)
    (iterate! doms (- nnodes 1))
    (list-ec (: i (- nnodes 1))
             (list (id->node i) (id->node (vector-ref doms i))))))
