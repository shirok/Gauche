;;;
;;; tests for lang.*
;;;
;;;   For now, these are experimental modules so we only test basic
;;;   stuff.  Eventually we need comprehensive tests for each lang.* modules.
;;;

(use gauche.test)
(test-start "lang.*")

(use lang.asm.x86_64)
(test-module 'lang.asm.x86_64)

(use lang.c.lexer)
(test-module 'lang.c.lexer)

(use lang.c.parser)
(test-module 'lang.c.parser)

(test-end)
