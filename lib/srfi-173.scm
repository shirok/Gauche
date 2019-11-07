;;;
;;; srfi-173 hooks
;;;

(define-module srfi-173
  (use gauche.hook)
  (export make-hook hook? list->hook list->hook!
          hook-add! hook-delete! hook-reset!
          hook->list hook-run))
(select-module srfi-173)

;; The following procedures are the same as gauche.hook
;; make-hook  hook?  hook->list

(define (hook-add! hook proc) (add-hook! hook proc))
(define (hook-delete! hook proc) (delete-hook! hook proc))
(define (hook-reset! hook) (reset-hook! hook))

(define (list->hook arity lst)
  (rlet1 hook (make-hook arity)
    (dolist [p lst] (add-hook! hook p))))
(define (list->hook! hook lst)
  (reset-hook! hook)
  (dolist [p lst] (add-hook! hook p)))

(define (hook-run hook . args) (apply run-hook hook args))
