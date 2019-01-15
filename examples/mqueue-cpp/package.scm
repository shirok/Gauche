(define-gauche-package "mqueue-cpp"
  :version "1.0"
  :description "Simple message queue\n\
          This is a sample package to show how to write Gauche extension\n\
          in C++."
  :require (("Gauche" (>= "0.9.7")))
  :providing-modules (example.mqueue-cpp)
  :authors ("Shiro Kawai <shiro@acm.org>")
  :licenses ("Public Domain")
  :homepage "https://practical-scheme.net/gauche"
  :repository "https://github.com/Gauche"
  )
