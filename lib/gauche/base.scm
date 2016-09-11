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
;;
;; NB: we also include gauche.keyword exported symbols in gauche.base; with
;; that, R7RS programs that say (import (gauche base)) can call Gauche
;; procedures with keyword arguments just like Gauche.  The downside is
;; that the set of exported identifiers from (gauche base) becomes open---
;; it grows as more keywords are registered.  I can't think of any concrete
;; case in which it becomes an issue, but R[67]RS library systems sort of
;; assume closed modules (you can determine set of identifiers to import
;; statically), so this might be a problem in some weired use case.

(define-module gauche.base
  (define-macro (do-export)
    (define all-symbols
      (remove (cut memq <> '(import))
              (hash-table-keys (module-table (find-module 'gauche)))))
    `(export (rename import gauche:import)
             ,@all-symbols))
  (extend gauche gauche.keyword)
  (do-export))

