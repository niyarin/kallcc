(include "./onif-symbol.scm")
(include "./onif-scm-env.scm");

(define-library (onif expand)
   (cond-expand
     ((library (srfi 125))
         (import (scheme base)
                 (scheme write);DEBUG
                 (srfi 125)
                 (onif scm env)
                 (onif symbol)))
     ((library (scheme hash))
         (import (scheme base)
                 (scheme hash)
                 (onif scm env)
                 (onif symbol)))
     (else
         (syntax-error "This scheme imprementation is not supported.")))

   (export onif-expand
           onif-expand-environment
           onif-expand/remove-outer-begin)

   (begin

     (define (%ref-environment-prefix sym prefix-symbol))

     (define (%rename-symbol sym))

     (define (%global-lookup symbol global)
         (if (hash-table-exists? global symbol)
           (hash-table-ref global symbol)
           (list 'undefined symbol)))

     (define (%local-cell-lookup symbol local-cell)
       (cond
         ((assv local-cell symbol) => cadr)
         (else
           #f)))

     (define (%lookup-environment symbol global stack )
         (let loop ((stack stack))
            (cond
              ((null? stack)
               (%global-lookup symbol global))
              ((%local-cell-lookup symbol (car stack)))
              (else
                (loop (cdr stack))))))

     (define (%expand-environment-ref symbol expand-environment)
       (cond
          ((assv symbol expand-environment) => cadr)
          ;error
          ))

     (define (%expand-environment-syntax-symbol-hash-ref
               symbol
               expand-environment)
       (let ((symbol-hash
               (%expand-environment-ref
                 'syntax-symbol-hash
                 expand-environment)))
         (hash-table-ref symbol-hash symbol)))

     (define (%list-expand-if scm-code global stack expand-environment)
       (cons
         (cadr (%expand-environment-syntax-symbol-hash-ref
                  'if
                  expand-environment))
         (map
           (lambda (expression)
             (onif-expand
               expression
               global
               stack
               expand-environment))
           (cdr scm-code))))

     (define (%list-expand-lambda scm-code global
                                  stack expand-environment)
       (let* ((bodies
                (map (lambda (expression)
                       (onif-expand
                         expression
                         global
                         stack
                         expand-environment))
                     (cddr scm-code)))
             (body
               (if (null? (cdr bodies))
                 (car bodies)
                 (cons
                   (cadr
                      (%expand-environment-syntax-symbol-hash-ref
                        'begin
                        expand-environment))
                   bodies))))
          (list
            (cadr
               (%expand-environment-syntax-symbol-hash-ref
                     'lambda
                     expand-environment))
            (cadr scm-code)
            body)))

     (define (%list-expand-begin scm-code global stack expand-environment)
       (cons (cadr (%expand-environment-syntax-symbol-hash-ref
                        'begin
                        expand-environment))
             (map (lambda (x)
                    (onif-expand x global stack expand-environment))
                  (cdr scm-code))))

     (define (%list-expand operator scm-code global stack expand-environment)
        "operator is pair. example. (built-in-lambda . ONIF-SYMBOL)"
         (let ((operator-kind (car operator)))
           (case operator-kind
              ((built-in-lambda);LAMBDA
               (%list-expand-lambda
                 scm-code
                 global
                 stack
                 expand-environment))
              ((built-in-if)
               (%list-expand-if
                 scm-code
                 global
                 stack
                 expand-environment))
              ((built-in-car built-in-cdr built-in-cons)
               (cons
                 (cadr operator)
                 (map
                    (lambda (expression)
                      (onif-expand
                        expression
                        global
                        stack
                        expand-environment))
                     (cdr scm-code))))
              ((built-in-begin)
               (%list-expand-begin
                 scm-code
                 global
                 stack
                 expand-environment))
              (else ;FUNC RUN
                  (map
                    (lambda (expression)
                      (onif-expand
                        expression
                        global
                        stack
                        expand-environment))
                  scm-code)))))

     (define (onif-expand scm-code global stack expand-environment)
        "global is scheme hash."
         (cond
           ((and (pair? scm-code) (symbol? (car scm-code)))
            (let ((operator (%lookup-environment (car scm-code) global stack)))
               (%list-expand operator scm-code global stack expand-environment)))
           ((and (pair? scm-code) (list? (car scm-code)))
            )
           (else
             scm-code)))

     (define (onif-expand/remove-outer-begin scm-code global)
       (let remove-begin ((code scm-code))
         (cond
           ((not (pair? code))
            (list code))
           ((eq? 'built-in-begin
                (car (%lookup-environment (car code) global '())))
            (apply append
                   (map remove-begin (cdr code))))
           (else
             (list code)))))

     (define (onif-expand-environment)
         `((syntax-symbol-hash
             ,(onif-scm-env-tiny-core))))))
