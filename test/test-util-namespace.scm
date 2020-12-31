(include "../src/kallcc-util-namespace.scm")

(import (scheme base)
        (srfi 64)
        (scheme hash-table)
        (scheme comparator)
        (scheme write)
        (prefix (kallcc util namespace) kunamespace/))

(test-begin "filter-keys-test")
(let ((global (hash-table (make-eq-comparator) 'a 1 'b 2 'c 3))
      (ans (hash-table (make-eq-comparator) 'a 1 'b 2)))
  (test-assert (hash-table=? (make-eq-comparator) (kunamespace/filter-keys global '(a b)) ans)))
(test-end "filter-keys-test")


(test-begin "merge-test")
(let ((g1 (hash-table (make-eq-comparator) 'a 1 'b 2))
      (g2 (hash-table (make-eq-comparator) 'c 3))
      (ans (hash-table (make-eq-comparator) 'a 1 'b 2 'c 3)))
  (test-assert (hash-table=? (make-eq-comparator) (kunamespace/merge g1 g2) ans)))
(test-end "merge-test")

