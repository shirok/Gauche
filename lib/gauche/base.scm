;;;
;;; gauche.base - a module to export Gauche builtin bindings
;;;

;; Module 'gauche' doesn't export any symbols, for it is supposed
;; to be accessed via inheritance.  So, we can't say (import (gauche)) in
;; order to access Gauche's builtins.
;;
;; Exporting symbols from 'gauche' has an unwanted side effect---since most
;; modules inherits 'gauche', using such module would import all symbols
;; from gauche as well, which would cause problems if any of use forms
;; before that was overriding gauche's default binding.  For example, suppose
;; you have the following code:
;;   (define-module foo (use gauche.sequence) (use gauche.parameter))
;; Although gauche.sequence provides overloaded 'map', it would be shadowed
;; by gauche.parameter if 'gauche' exports the original 'map'.
;;
;; So here's a new module gauche.base, which is just to export Gauche's
;; builtin functions.  R7RS program can say (import (gauche base)) to access
;; them.  Note that we rename gauche#import to gauche:import, to avoid
;; shadowing r7rs#import.
;;
;; NB: gauche.base does not export bindings inherited from #<module scheme>
;; or #<module null>

(define-module gauche.base
  (define-macro (do-export)
    (define all-symbols
      (remove (cut memq <> '(import))
              (hash-table-keys (module-table (find-module 'gauche)))))
    `(export (rename import gauche:import)
             ,@all-symbols))
  (do-export))

