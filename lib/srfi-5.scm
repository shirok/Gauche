;;
;; SRFI-5: A compatible let form with signatures and rest arguments
;;

;; This implementation is based on Andy Gaynor's reference implementation.

(define-module srfi-5
  (export let))
(select-module srfi-5)

(define-syntax let
  (syntax-rules ()
    ;; standard-compatible lambdas.
    ((let () body ...)
     ((lambda () body ...)))
    ((let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))

    ;; let with rest parameter
    ((let ((var val) . bindings) body ...)
     (%let-loop #f bindings (var) (val) (body ...)))

    ;; signature-style name.
    ((let (name binding ...) body ...)
     (%let-loop name (binding ...) () () (body ...)))

    ;; standrad named let (which may have rest parameter)
    ((let name bindings body ...)
     (%let-loop name bindings () () (body ...)))

    ;; error
    ((let . _)
     (syntax-error "malformed let:" (let . _)))
    ))

;; aux macro to collect bindings
(define-syntax %let-loop
  (syntax-rules ()
    ((%let-loop name ((var0 val0) binding ...) (var ...) (val ...) body)
     (%let-loop name (binding ...) (var ... var0) (val ... val0) body))

    ;; rest binding, no name
    ((%let-loop #f (rest-var rest-val ...) (var ...) (val ...) body)
     ((lambda (var ... . rest-var) . body) val ... rest-val ...))

    ;; no bindings, named
    ((%let-loop name () (var ...) (val ...) body)
     ((letrec ((name (lambda (var ...) . body)))
        name)
      val ...))

    ;; rest binding, named
    ((%let-loop name (rest-var rest-val ...) (var ...) (val ...) body)
     ((letrec ((name (lambda (var ... . rest-var) . body)))
        name)
      val ... rest-val ...))))

