(include "../src/kallcc-tree.scm")

(import (scheme base)
        (srfi 64)
        (prefix (kallcc tree) ktree/))

(begin;;tconc-test
   (test-begin "test-any")
   (test-assert (ktree/any integer? '(1 2 3)))
   (test-assert (ktree/any integer? '((1 2 3))))
   (test-assert (ktree/any integer? '(hello (1 2 3))))
   (test-end "test-any"))


