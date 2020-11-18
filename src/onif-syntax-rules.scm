(include "./lib/rules.scm")

(define-library (onif syntax-rules)
   (import (scheme base)
           (niyarin rules))
   (export onif-syntax-rules/make-syntax-rules onif-syntax-rules/expand)
   (begin
      (define-record-type <syntax-rules-object>
         (%make-syntax-rules-object ellipsis literals rules)
         syntax-rules-object?
         (ellipsis %ellipsis)
         (literals %literals)
         (rules %rules))

      (define (onif-syntax-rules/make-syntax-rules ellipsis literals rules)
        (%make-syntax-rules-object ellipsis literals rules))

      (define (onif-syntax-rules/expand rules-object input)
        ;;NOT HYGENIC!
        (let loop ((rules (%rules rules-object)))
           (let ((m (rules/match (%ellipsis rules-object)
                                 (%literals rules-object)
                                 (caar rules)
                                 input)))
             (if m (rules/expand (cadr (car rules)) m) (loop (cdr rules))))))))
