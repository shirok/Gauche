(use gauche.test)

(test-start "data.* extensions")

(include "test-queue.scm")
(include "test-ring-buffer.scm")
(include "test-trie.scm")
(include "test-random.scm")
(include "test-heap.scm")

(test-end)
