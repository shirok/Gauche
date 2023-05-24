;;
;; Compsite library of entire SRFI-226
;;

(define-module srfi.226
  (extend srfi.226.prompt
          srfi.226.continuation
          srfi.226.shift-reset
          srfi.226.inspection
          srfi.226.continuation-mark
          srfi.226.parameter
          srfi.226.fluid
          srfi.226.call-in-initial-continuation
          srfi.226.promise
          srfi.226.exception
          srfi.226.condition
          srfi.226.time
          srfi.226.thread
          srfi.226.thread-local
          srfi.226.interrupt))
