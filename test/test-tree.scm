(include "../src/kallcc-tree.scm")

(import (scheme base)
        (srfi 64)
        (scheme write)
        (prefix (kallcc tree) ktree/))

(begin;;any-test
   (test-begin "test-any")
   (test-assert (ktree/any integer? '(1 2 3)))
   (test-assert (ktree/any integer? '((1 2 3))))
   (test-assert (ktree/any integer? '(hello (1 2 3))))
   (test-assert (ktree/any integer? '(hello ((1 2 3)))))
   (test-assert (not (ktree/any integer? '(a b c))))
   (test-end "test-any"))

(begin;;update-test
   (test-begin "test-update")
   (test-equal (ktree/update integer? (lambda (x) (+ x 1)) '(1 2 3)) '(2 3 4))
   (test-equal (ktree/update integer? (lambda (x) (+ x 1)) '((1) (2) (3) (hello))) '((2) (3) (4) (hello)))
   (test-end "test-update"))

