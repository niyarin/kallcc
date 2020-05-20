(include "./onif-symbol.scm")
(include "./onif-scm-env.scm");
(include "./onif-misc.scm");
(include "./lib/thread-syntax.scm")
(include "./onif-idebug.scm")

;TODO:fix import expression

(define-library (onif expand)
   (cond-expand
     ((library (srfi 125))
         (import (scheme base)
                 (scheme write);DEBUG
                 (scheme cxr)
                 (srfi 125)
                 (srfi 1)
                 (niyarin thread-syntax)
                 (onif idebug);
                 (onif scm env)
                 (onif misc)
                 (onif symbol)))
     ((library (scheme hash))
         (import (scheme base)
                 (scheme list)
                 (scheme hash)
                 (onif scm env)
                 (onif symbol)))
     (else
         (syntax-error "This scheme imprementation is not supported.")))

   (export onif-expand
           onif-expand-environment
           onif-expand/remove-outer-begin
           onif-expand/separate-namespaces
           onif-expand/import-expression?
           onif-expand/export-expression?
           onif-expand/make-environment
           onif-expand/core-library-name?
           onif-expand/make-library-environment
           onif-expand/defined-symbols)

   (begin

     (define (%ref-environment-prefix sym prefix-symbol))

     (define (%rename-symbol sym))

     (define (%global-lookup symbol global) (if (hash-table-exists? global symbol)
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

     (define (%list-expand-define-library
               scm-code global stack expand-environment)
         (let ((new-global (onif-scm-env-tiny-core))
               (lib-name (cadr scm-code))
               (begin-expression
                 (cons 'begin
                       (cddr scm-code))))
           (%list-expand `(built-in-begin ,(onif-symbol 'BEGIN))
                         begin-expression
                         global
                         '()
                         expand-environment)))

     (define (%list-expand-begin scm-code global stack expand-environment)
       (cons (cadr (%expand-environment-syntax-symbol-hash-ref
                        'begin
                        expand-environment))
             (map (lambda (x)
                    (onif-expand x global stack expand-environment))
                  (cdr scm-code))))

     (define (%list-expand-define scm-code global stack expand-environment)
       ;;TODO: Add support for defining function syntax sugar
       `(,(cadr (%expand-environment-syntax-symbol-hash-ref
                     'define
                     expand-environment))
         ,(cadr scm-code)
         ,(onif-expand (caddr scm-code) global stack expand-environment)))

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
              ((built-in-define)
               (%list-expand-define
                 scm-code
                 global
                 stack
                 expand-environment))
              ((built-in-quote)
                  (list (cadr (%expand-environment-syntax-symbol-hash-ref
                              'quote
                              expand-environment))
                        (cadr scm-code)))
              ((built-in-car built-in-cdr built-in-cons built-in-null?)
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
              ((built-in-define-library)
               (%list-expand-define-library
                 scm-code
                 global
                 stack
                 expand-environment))
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
           ((not-pair? code)
            (list code))
           ((eq? 'built-in-begin
                (car (%lookup-environment (car code) global '())))
            (append-map remove-begin (cdr code)))
           (else
             (list code)))))

     (define (onif-expand/separate-namespaces expressions global)
       (let-values (((namespaces this-expressions)
                        (onif-misc/filter-&-elses
                          (lambda (x)
                            (and (pair? x)
                                 (eq? 'built-in-define-library
                                      (car (%lookup-environment
                                             (car x)
                                             global
                                             '())))))
                          expressions)))
            (cons (list '() this-expressions)
                  (map cdr namespaces))))

     (define (onif-expand/import-expression? expression global)
         (and (pair? expression)
              (let ((ope (%lookup-environment
                           (car expression)
                           global
                           '())))
                (and (pair? ope)
                     (eq? 'built-in-import
                          (car ope))))))

      (define (onif-expand/export-expression? expression global)
         (and (pair? expression)
              (let ((ope (%lookup-environment
                           (car expression)
                           global
                           '())))
                (and (pair? ope)
                     (eq? 'built-in-export
                          (car ope))))))

     (define (onif-expand-environment)
         `((syntax-symbol-hash
             ,(onif-scm-env-tiny-core))))

     (define (onif-expand/make-library-environment)
         `((syntax-symbol-hash
             ,(onif-scm-env/make-env-for-library))))


     (define (onif-expand/core-library-name? lib-name)
       (equal? lib-name '(onif-lib core)))

     (define (onif-expand/make-environment lib-names)
       (fold
         (lambda (lib-name env)
           (cond
             ((onif-expand/core-library-name? lib-name)
              (hash-table-merge! env (onif-scm-env-tiny-core)))
             (else
               env)))
         (make-hash-table eq?)
         lib-names))

     (define (onif-expand/defined-symbols expressions symbol-hash)
       (->> expressions
            (map (lambda (expression)
                    (cond
                      ((not-pair? expression) #f)
                      ((onif-misc/define-operator?
                         (car expression) symbol-hash)
                       (cadr expression))
                      (else #f))))
             (filter (lambda (x) x))))))
