(use gauche.test)

(test-start "data.* extensions")

(include "test-queue.scm")
(include "test-ring-buffer.scm")
(include "test-trie.scm")

(test-end)

