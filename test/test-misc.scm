(include "../src/kallcc-misc.scm")

(import (scheme base)
        (srfi 64)
        (scheme set)
        (scheme comparator)
        (prefix (kallcc misc) kallcc-misc/))

(begin;;tconc-test
   (test-begin "test-tconc")
   (let ((tconc (kallcc-misc/make-tconc)))
     (kallcc-misc/tconc-push! tconc 1)
     (test-equal (kallcc-misc/tconc-head tconc) '(1))
     (kallcc-misc/tconc-push! tconc 2)
     (test-equal (kallcc-misc/tconc-head tconc) '(1 2)))
   (test-end "test-tconc"))

(begin;;scm-expression->symbol-set-test
   (test-begin "test-scm-expression->symbol-set")
   (set=? (kallcc-misc/scm-expression->symbol-set '(list 1 2 a b "c"))
          (set (make-eq-comparator) '(list a b)))

   (set=? (kallcc-misc/scm-expression->symbol-set '(list (cons a b) (list c d)))
          (set (make-eq-comparator) '(list cons a b c d)))
   (test-end "test-scm-expression->symbol-set"))
