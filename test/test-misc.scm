(include "../src/kallcc-misc.scm")

(import (scheme base)
        (srfi 64)
        (prefix (kallcc misc) kallcc-misc/))

(begin;;tconc-test
   (test-begin "test-tconc")
   (let ((tconc (kallcc-misc/make-tconc)))
     (kallcc-misc/tconc-push! tconc 1)
     (test-equal (kallcc-misc/tconc-head tconc) '(1))
     (kallcc-misc/tconc-push! tconc 2)
     (test-equal (kallcc-misc/tconc-head tconc) '(1 2)))
   (test-end "test-tconc"))
