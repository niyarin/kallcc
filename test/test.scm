(include "./onif-test-expand.scm")
(include "./onif-test-utils.scm")

(import 
  (scheme base)
  (srfi 78) ;Lightweight testing
  (onif test expand)
  (onif test utils))

(onif-test-expand)
(onif-test-utils)
