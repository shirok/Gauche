;;
;; This script is to run the extension in part of `DIST self-host-test`.
;; See DIST script for the details.
;;

(use gauche.configure)
(use gauche.process)

(cf-init-gauche-extension)
(cf-check-headers '("uuid/uuid.h"))
(define *have-libuuid* #f)
(cf-search-libs "uuid_clear" '("uuid")
                :if-found (^z  ; can be #f, if uuid_clear is available by default
                            (set! *have-libuuid* (boolean z))))

;; If libuuid isn't available, just skip the test.
(unless *have-libuuid*
  (print "No libuuid - skipping ffi-libuuid test")
  (exit 0))

(cf-output-default)
(run-process '(make -s check) :fork #f)
