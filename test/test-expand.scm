(include "../src/onif-expand.scm")

(import (scheme base)
        (srfi 64)
        (scheme write)
        (onif expand)
        (onif scm env))

(test-begin "make-syntax-object-test")
(let ((global (onif-scm-env/tiny-core)))
  (let ((sym-expand (onif-expand/make-syntax-object 'define global)))
    (test-eq (car sym-expand) 'built-in-define)))
(test-end "make-syntax-object-test")
