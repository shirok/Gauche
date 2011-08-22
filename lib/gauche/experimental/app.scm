;; The '$' macro is incorporated in the core.  (lib/gauche/common-macros.scm)

(define-module gauche.experimental.app
  ;; This is for compatibility - while compiling 0.9.3 we need to use 0.9.2,
  ;; which needs gauche.experimental.app to provide '$'.
  ;; Actually, including common-macros.scm inserts all the bindings to
  ;; 'gauche' module so it is differnet from having gauche.experimental.app
  ;; to provide '$'.  But the difference shouldn't matter for the build process
  ;; to work.
  (include "../common-macros.scm"))


