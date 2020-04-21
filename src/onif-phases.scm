(include "./onif-expand.scm")
(include "./onif-symbol.scm")
(include "./lib/thread-syntax.scm")
(include "./onif-idebug.scm")
(include "./onif-scm-env.scm")
(include "./onif-cps.scm")
(include "./onif-meta-lambda.scm")
(include "./onif-flat-lambda.scm")
(include "./onif-name-ids.scm")
(include "./onif-alpha-conv.scm")
(include "./onif-like-asm.scm")

(define-library (onif phases)
   (import (scheme base)
           (onif misc)
           (scheme cxr)
           (scheme write);
           (onif idebug)
           (srfi 1)
           (srfi 69);
           (only (niyarin thread-syntax) ->> ->)
           (onif symbol)
           (onif scm env)
           (onif meta-lambda)
           (onif name-ids)
           (onif alpha conv)
           (onif flat-lambda)
           (onif like-asm)
           (onif expand)
           (onif cps))
   (export onif-phases/pre-expand
           onif-phases/expand-namespaces
           onif-phases/solve-imported-name
           onif-phases/solve-imported-symbol
           onif-phases/alpha-conv!
           onif-phases/first-stage
           onif-phase/meta-lambda
           onif-phase/make-name-local-ids
           onif-phase/flat-lambda
           onif-phases/cps-conv
           onif-phase/asm)
   (begin
     (define (onif-phases/pre-expand code global)
       (let* ((expression-begin-removed
                (apply
                  append
                  (map (lambda (expression)
                         (onif-expand/remove-outer-begin expression global))
                       code)))
              (namespaces
                 (onif-expand/separate-namespaces
                   expression-begin-removed
                   global)))
             namespaces))

      (define (%expand-loop expressions global expand-environment other-namespaces)
        ;TODO: import macros from other namespaces
        (let ((res (onif-misc/ft-pair)))
           (let loop ((expressions expressions)
                      (global global)
                      (expand-environment expand-environment))
             (cond
               ((null? expressions)
                (cons expand-environment
                     (onif-misc/ft-pair-res res)))
               ((onif-expand/import-expression?  (car expressions) global)
                (let* ((new-global (onif-expand/make-environment (cdar expressions)))
                       (new-expand-environment
                         (list (list 'syntax-symbol-hash new-global)
                               (list 'original-syntax-symbol-hash (onif-scm-env-tiny-core))
                               (list 'import-libraries
                                     (filter (lambda (libname) (not (onif-expand/core-library-name? libname)))
                                             (cdar expressions))))))
                   (loop (cdr expressions)
                         new-global
                         new-expand-environment)))
               ((onif-expand/export-expression? (car expressions) global)
                   (loop (cdr expressions);TODO: Support rename
                         global
                         (cons `(export-symbols ,(cdar expressions))
                               expand-environment)))
               (else
                 (onif-misc/ft-pair-push!
                   res
                   (onif-expand (car expressions) global '() expand-environment))
                 (loop (cdr expressions) global expand-environment))))))

      (define (onif-phases/expand-namespaces this-expressions namespaces expand-environment)
        (->> (append namespaces (list (cons '() this-expressions)))
             (fold (lambda (namespace res)
                     (let* ((pre-expanded-expressions
                                 (onif-phases/pre-expand (cdr namespace)
                                                         (cadr (assq 'syntax-symbol-hash
                                                         expand-environment))))
                            (target-expressions (cadar pre-expanded-expressions)))
                        (cons (cons (car namespace)
                                    (%expand-loop target-expressions
                                                  (cadr (assq 'syntax-symbol-hash expand-environment))
                                                  expand-environment
                                                  res))
                              res)))
                   '())))

      (define (onif-phases/alpha-conv! expanded-namespaces symbol-hash)
        (let* ((defined-symbols
                   (->> expanded-namespaces
                        (map (lambda (namespace-expression)
                               (->> (onif-expand/defined-symbols (cddr namespace-expression) symbol-hash)
                                    (map (lambda (sym)
                                           (list sym (onif-symbol sym))))))))))
          (for-each (lambda (expressions renamed-targets)
                      (begin
                           (set-car! (cdr expressions)
                                     (cons `(export-rename ,renamed-targets)
                                           (cadr expressions)))
                         (->> (cddr expressions)
                              (for-each
                                (lambda (expression)
                                   (onif-alpha/conv!
                                     expression
                                     (->> (assq 'syntax-symbol-hash (cadr expressions))
                                          cadr)
                                     (list renamed-targets)))))))
               expanded-namespaces
               defined-symbols)))

      (define (%namespace-assq key namespace . default)
        (cond
          ((assq key (cadr namespace)) => cadr)
          ((not (null? default)) (car default))
          (else (error "Namespace doesn't have key."
                       key
                       ;(->> (cadr namespace) (map car))
                       namespace
                       ;(cadr namespace)
                       ))))

      (define (onif-phases/solve-imported-symbol namespaces)
        (->> namespaces
             (map (lambda (namespace)
                    (let* ((import-libnames (%namespace-assq 'import-libraries namespace))
                           (import-symbols
                             (->> import-libnames
                                  (map (lambda (libname)
                                          (->> (assv libname namespaces)
                                               (%namespace-assq
                                                 'export-rename))))
                                  (apply append))))
                       import-symbols)))))

      (define (onif-phases/first-stage code expand-environment)
         (let* ((global
                  (cadr (assq 'syntax-symbol-hash expand-environment)))
                (namespaces
                  (onif-phases/pre-expand code (onif-scm-env-tiny-core)))
                (expanded-namespaces
                  (onif-phases/expand-namespaces (cadar namespaces) (cdr namespaces) expand-environment))
                (alpha-converted-code
                  (begin (onif-phases/alpha-conv! expanded-namespaces global)
                         expanded-namespaces))
               (namespaces
                   (->> alpha-converted-code
                     (map (lambda (namespace)
                            (list
                              (car namespace)
                              (cons `(body ,(cddr namespace))
                                    (cons `(name ,(car namespace))
                                          (cadr namespace))))))))
               (imported-rename
                 (begin
                    (map (lambda (expressions rename-alist)
                           (->> (cadr expressions)
                                (assv 'body )
                                cadr
                                (for-each (lambda (expression)
                                              (onif-alpha/simple-conv!
                                                expression
                                                rename-alist)))))
                         namespaces
                         (onif-phases/solve-imported-symbol namespaces))
                    namespaces)))
            imported-rename))

      (define (onif-phases/cps-conv namespaces)
         (->> namespaces
              (map (lambda (namespace)
                     (->> (%namespace-assq 'body namespace)
                          (map (lambda (expression)
                                  (onif-cps-conv
                                    expression
                                    (%namespace-assq
                                      ;'original-syntax-symbol-hash
                                      'syntax-symbol-hash
                                      namespace))))
                         ((lambda (expression)
                            (list (car namespace)
                                  (cons (list 'body expression)
                                     (cadr namespace))))))))))

      (define (onif-phase/meta-lambda namespaces)
        (->> namespaces
             (map (lambda (namespace)
                    (->> (%namespace-assq 'body namespace)
                         (map (lambda (expression)
                                  (onif-meta-lambda-conv
                                      expression
                                      (%namespace-assq
                                        'syntax-symbol-hash
                                        namespace)
                                      (cadr namespace))))
                         ((lambda (expression)
                             (list (car namespace)
                                   (cons (list 'body expression)
                                         (cadr namespace))))))))))

      (define (onif-phase/make-name-local-ids namespaces)
        (->> namespaces
             (map (lambda (namespace)
                    (->> (%namespace-assq 'body namespace)
                         (map (lambda (expression)
                                 (name-ids/make-name-local-ids
                                    expression
                                    (%namespace-assq 'syntax-symbol-hash namespace))))
                         ((lambda (expression)
                            (list (car namespace)
                                  (cons (list 'body expression)
                                        (cadr namespace))))))))))

      (define (onif-phase/flat-lambda namespaces)
        (let ((res (onif-misc/ft-pair)))
          (fold (lambda  (namespace offset)
                   (let ((symbol-hash
                           (%namespace-assq 'syntax-symbol-hash namespace)))
                      (->> (%namespace-assq 'body  namespace)
                           (fold (lambda (expression expression-offset)
                                   (let-values (((flat-code _id-lambdas)
                                                 (onif-flat-flat-code&id-lambdas
                                                   expression
                                                   (->> (map cadr expression-offset)
                                                        (apply append)
                                                        length
                                                        (+ offset))
                                                   symbol-hash
                                                   (cadr namespace))))
                                      (cons (list flat-code _id-lambdas)
                                            expression-offset)))
                                 '())
                           ;Current data format is ((flat code id-lambdas) ...)
                           ((lambda (expression-offsets)
                              (let ((lambdas (apply append (map cadr expression-offsets))))
                                 (onif-misc/ft-pair-push!
                                   res
                                   `(,(car namespace)
                                      ((body ,(apply append (map car expression-offsets)))
                                       (id-lambdas ,lambdas)
                                       .
                                       ,(cadr namespace))))

                                 (+ (length lambdas) offset)))))))
             0 namespaces)
          (onif-misc/ft-pair-res res)))))
