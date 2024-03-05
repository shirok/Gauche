(define-gauche-package "example-spigot"
  :version "1.0"
  :description "Calculate pi and e in arbitrary precision\n\
           This is a sample package to show how to write Gauche extension.\n\
           The C code implements Spigot algorithm."
  :require (("Gauche" (>= "0.9.7")))
  :providing-modules (math.spigot)
  :authors ("Shiro Kawai <shiro@acm.org>")
  :licenses ("Public Domain")
  :homepage "https://practical-scheme.net/gauche"
  :repository "https://github.com/Gauche"
  )
