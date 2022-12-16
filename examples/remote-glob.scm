;; Run glob on remote machine
;;
;; It shows how to customize glob.
;; This isn't a best way to implement remote glob, though, for it needs
;; to call remote process on every path component.

(use gauche.process)
(use file.util :only (build-path))
(use srfi.13 :only (string-trim-right string-drop-right))

(define (make-remote-folder host)
  (^[proc seed parent regexp non-leaf?]
    (let1 prefix (case parent
                   [(#t) "/"]
                   [(#f) "./"]
                   [else (string-append (string-trim-right parent #\/))])
      (cond [(eq? regexp 'dir)              ; returns the directory itself
             (proc prefix seed)]
            [non-leaf?
             ($ fold proc seed
                $ filter-map (^p (and (regexp p) (build-path prefix p)))
                $ filter-map (^p (and (#/\/$/ p) (string-drop-right p 1)))
                $ process-output->string-list `(ls -F ,prefix) :host host)]
            [else
             ($ fold proc seed
                $ filter-map (^p (and (regexp p) (build-path prefix p)))
                $ process-output->string-list `(ls ,prefix) :host host)]))))

(define (remote-glob host pattern)
  (glob pattern :folder (make-remote-folder host)))
