(include "./lib/rules.scm")

(define-library (onif syntax-rules)
   (import (scheme base)
           (niyarin rules))
   (export onif-syntax-rules/make-syntax-rules onif-syntax-rules/expand
           onif-syntax-rules/ref-global onif-syntax-rules/ref-stack)
   (begin
      (define-record-type <syntax-rules-object>
         (%make-syntax-rules-object ellipsis literals rules global stack)
         syntax-rules-object?
         (ellipsis %ellipsis)
         (literals %literals)
         (rules %rules)
         (global %global)
         (stack %stack))

      (define (onif-syntax-rules/make-syntax-rules ellipsis literals rules global stack)
        (%make-syntax-rules-object ellipsis literals rules global stack))

      (define onif-syntax-rules/ref-global %global)
      (define onif-syntax-rules/ref-stack %stack)

      (define (onif-syntax-rules/expand rules-object input)
        ;;NOT HYGENIC!
        (let loop ((rules (%rules rules-object)))
           (let ((m (rules/match (%ellipsis rules-object)
                                 (%literals rules-object)
                                 (caar rules)
                                 input)))
             (if m (rules/expand (cadr (car rules)) m) (loop (cdr rules))))))))
