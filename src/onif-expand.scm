(include "./onif-symbol.scm")
(include "./onif-scm-env.scm");
(include "./onif-misc.scm");
(include "./lib/thread-syntax.scm")
(include "./lib/scmspec.scm")
(include "./onif-syntax-rules.scm")
(include "./kallcc-util-namespace.scm")

;TODO:fix import expression
(define-library (onif expand)
   (import (scheme base)  (scheme list) (scheme set)
           (scheme hash-table) (scheme comparator)
           (scheme cxr)
           (onif scm env) (onif misc) (onif symbol)
           (onif syntax-rules)
           (onif idebug)
           (niyarin thread-syntax)
           (prefix (kallcc misc) kmisc/)
           (prefix (kallcc symbol) ksymbol/)
           (prefix (kallcc util namespace) kunamespace/)
           (scmspec core)
           (scheme write);DEBUG
           )

   (export onif-expand
           onif-expand/make-syntax-object
           onif-expand/expand
           onif-expand-environment
           onif-expand/remove-outer-begin
           onif-expand/separate-namespaces
           onif-expand/import-expression?
           onif-expand/export-expression?
           onif-expand/make-environment
           onif-expand/core-library-name?
           onif-expand/make-library-environment
           onif-expand/defined-symbols
           onif-expand/define-syntax-expression?)

   (begin

     (define (%ref-environment-prefix sym prefix-symbol))

     (define (%rename-symbol sym))

     (define (%global-lookup symbol global)
       (if (hash-table-exists? global symbol)
           (hash-table-ref global symbol)
           (list 'undefined symbol)))

     (define (%local-cell-lookup symbol local-cell)
       (cond
         ((assq symbol local-cell) => cadr)
         (else #f)))

     (define (%lookup-environment symbol global stack)
         (let loop ((stack stack))
            (cond
              ((null? stack) (%global-lookup symbol global))
              ((%local-cell-lookup symbol (car stack)))
              (else (loop (cdr stack))))))

     (define (%local-var? symbol stack)
       (let loop ((stack stack))
         (cond
           ((null? stack) #f)
           ((%local-cell-lookup symbol (car stack)) #t)
           (else (loop (cdr stack))))))

     (define (%expand-environment-ref symbol expand-environment)
       (cond
          ((assv symbol expand-environment) => cadr)
          ;error
          ))

     (define-record-type <onif-macro>
         (%onif-macro rule-code)
         %onif-macro?
         (rule-code %macro-ref-rule-code %macro-set-rule-code!))

     (define (%expand-environment-syntax-symbol-hash-ref
               symbol
               expand-environment)
       (let ((symbol-hash
               (%expand-environment-ref
                 'original-syntax-symbol-hash
                 expand-environment)))
         (hash-table-ref symbol-hash symbol)))

     (define (%list-expand-if scm-code global stack expand-environment)
       (cons
         (cadr (%expand-environment-syntax-symbol-hash-ref
                  'if
                  expand-environment))
         (map (lambda (expression)
                 (onif-expand expression global
                              stack expand-environment))
              (cdr scm-code))))

     (define (%dot-or-list->list formals)
       (let loop ((fmls formals)
                  (res '()))
         (cond
           ((null? fmls) res)
           ((not-pair? fmls) (cons fmls res))
           (else (loop (cdr fmls) (cons (car fmls) res))))))

     (define (%list-expand-lambda scm-code global
                                  stack expand-environment)
       ;;stackはlist of alist
       ;(list 'undefined symbol)
       (let* ((args-list (%dot-or-list->list (cadr scm-code)))
              (stack-cell (map (lambda (x) (list x (list 'undefined x)))
                               args-list))
              (bodies
                (map (lambda (expression)
                       (onif-expand expression global (cons stack-cell stack)
                                    expand-environment))
                     (cddr scm-code)))
              (body
                (if (null? (cdr bodies))
                  (car bodies)
                  (cons (cadr (%expand-environment-syntax-symbol-hash-ref
                                 'begin expand-environment))
                        bodies))))
          (list (cadr (%expand-environment-syntax-symbol-hash-ref
                     'lambda expand-environment))
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

     (define (%list-expand-set! scm-code global stack expand-environment)
       `(,(cadr (%expand-environment-syntax-symbol-hash-ref
                     (if (%local-var? (cadr scm-code) stack) 'local-set! 'set!)
                     expand-environment))
         ,(cadr scm-code)
         ,(onif-expand (caddr scm-code) global stack expand-environment)))

     (define (%expr->symbol-rename-alist expr symbol-proc)
      (map (lambda (x) (cons x (ksymbol/kallcc-symbol x)))
           (set->list (kmisc/scm-expression->symbol-set expr symbol-proc))))

     (define (%make-hygenic-symbol sym global stack global* stack*)
       (let ((local-var1
               (car (filter-map (lambda (cell) (assq sym cell))
                                stack)))
             (local-var2
                   (filter-map (lambda (cell) (assq sym cell))
                               stack*)))
         (cond
           ((and (null? local-var1)
                 (null? local-var2)
                 (eq? global global*))
            sym)
           ((eq? global global*) `(global ,(car local-var1)))
           (else (error "WIP!")))))

     (define (%make-hygenic-rename-alist symbols global stack global* stack* expand-environment)
       (map (lambda (sym)
              (let ((hsym (%make-hygenic-symbol sym global stack global* stack*)))
                (if (pair? hsym)
                  (cons sym
                        (list (cadr (%expand-environment-syntax-symbol-hash-ref
                                      'ref-var expand-environment))
                              hsym))
                  (cons sym hsym))))
            symbols))

     (define (%expand-user-syntax expr srules-object global
                                  stack expand-environment)
       ;;stackにつっこむのをわすれるな
       ;;synatax-globalを使うこと
       (let* ((syntax-stack  (onif-syntax-rules/ref-stack srules-object))
              ;入力のシンボルを別の名に新しい置き換える(辞書作成)
              (evacuation-symbol-alist (%expr->symbol-rename-alist
                                        expr kmisc/var?))
              ;退避リネーム
              (pre-renamed-expr
                (kmisc/rename-symbol-in-expression expr
                                                   evacuation-symbol-alist))
              ;マクロ展開
              (macro-expanded
                (onif-syntax-rules/expand srules-object pre-renamed-expr))

              ;let-syntaxは考慮しない
              (syntax-global (kunamespace/merge
                               (onif-syntax-rules/ref-global srules-object)
                               (alist->hash-table (map (lambda (key-val)
                                                           (cons (cdr (assq (car key-val) evacuation-symbol-alist))
                                                                 (cdr key-val)))
                                                       (kunamespace/filter-keys-alist global (map car evacuation-symbol-alist)))
                                                  (make-eq-comparator))))
              ;
              (expanded
                (onif-expand/expand macro-expanded syntax-global syntax-stack
                                    expand-environment))
              (after-symbol-dict
                (%make-hygenic-rename-alist
                  (kmisc/find-in-expression expanded symbol?)
                  global stack syntax-global syntax-stack expand-environment))
              (after-renamed-expr
                (kmisc/rename-symbol-in-expression expanded after-symbol-dict))
              (after-renamed-expr*
                (kmisc/rename-symbol-in-expression after-renamed-expr
                                                   (kmisc/inverse-alist evacuation-symbol-alist))))
        ;(display "1>>>")(display stack)(newline)
        ;(display "2>>>")(onif-idebug/debug-display after-renamed-expr*)(newline)
        ;;(display "3>>>")(onif-idebug/debug-display after-symbol-dict)(newline)
        ;(display "1>>>")(onif-idebug/debug-display (hash-table->alist syntax-global))(newline)
        ;(display "2>>>")(onif-idebug/debug-display macro-expanded)(newline)
        ;(display "3>>>")(onif-idebug/debug-display expanded)(newline)
        after-renamed-expr*
        ))

     (define (%list-expand operator scm-code global stack expand-environment)
       "operator is pair. example. (built-in-lambda . ONIF-SYMBOL)"
       (scmspec/lcheck ((input (operator (scmspec/pair symbol? scmspec/any?))))
          (let ((operator-kind (car operator)))
            (case operator-kind
               ((built-in-lambda);LAMBDA
                (%list-expand-lambda scm-code global stack expand-environment))
               ((built-in-if)
                (%list-expand-if scm-code global stack expand-environment))
               ((built-in-define)
                (%list-expand-define scm-code global stack expand-environment))
               ((built-in-set!)
                (%list-expand-set! scm-code global stack expand-environment))
               ((built-in-quote)
                   (list (cadr (%expand-environment-syntax-symbol-hash-ref
                               'quote
                               expand-environment))
                         (cadr scm-code)))
               ((built-in-syntax-error)
                  (erorr "Syntax error"))
               ((built-in-car built-in-cdr built-in-cons built-in-eq?
                 built-in-vector-set! built-in-make-vector built-in-vector-length built-in-vector-ref
                 built-in-fx+ built-in-fx- built-in-fx* built-in-fx=?
                 built-in-fxremainder
                 built-in-fx<?  built-in-make-bytevector
                 built-in-bytevector-u8-ref built-in-bytevector-u8-set!
                 built-in-bytevector-length)
                (cons
                  (cadr operator)
                  (map (lambda (expression)
                          (onif-expand
                            expression global stack expand-environment))
                      (cdr scm-code))))
               ((built-in-define-library)
                (%list-expand-define-library
                  scm-code global stack expand-environment))
               ((built-in-define-syntax)
                   (let ((syntax-object
                           (onif-expand/make-syntax-object
                             (list-ref scm-code 2) global stack)))
                     (hash-table-set! global (cadr scm-code) syntax-object)
                     #f))
               ((built-in-begin)
                (%list-expand-begin
                  scm-code global stack expand-environment))
               ((user-syntax)
                 (%expand-user-syntax scm-code (cadr operator) global
                                      stack expand-environment))
               (else (map (lambda (expression)
                             (onif-expand/expand expression global
                                          stack expand-environment))
                     scm-code))))))

     (define (onif-expand/expand scm-code global stack expand-environment)
        "global is scheme hash."
        (cond
           ;((symbol? scm-code) (%lookup-environment scm-code global stack))
           ((and (pair? scm-code) (kmisc/var? (car scm-code)))
            (let ((operator (%lookup-environment (car scm-code) global stack)))
               (%list-expand operator scm-code global stack expand-environment)))
           ((and (pair? scm-code) (list? (car scm-code)))
            ;;specを強引に回避しているので跡で見直す
            (%list-expand (cons '_ #f) scm-code global stack expand-environment))
           (else scm-code)))

     (define onif-expand onif-expand/expand)

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
                        (partition
                          (lambda (x)
                            (and (pair? x)
                                 (eq? 'built-in-define-library
                                      (car (%lookup-environment (car x) global '())))))
                          expressions)))
            (cons `(() ((body ,this-expressions)))
                  (map (lambda (define-library-code)
                         `(,(cadr define-library-code)
                           ((body ,(cddr define-library-code)))))
                       namespaces))))

     (define (onif-expand/import-expression? expression global)
       (and (pair? expression)
            (let ((ope (%lookup-environment (car expression) global '())))
              (and (pair? ope) (eq? 'built-in-import (car ope))))))

      (define (onif-expand/export-expression? expression global)
         (and (pair? expression)
              (let ((ope (%lookup-environment (car expression) global '())))
                (and (pair? ope) (eq? 'built-in-export (car ope))))))

      (define (onif-expand/define-syntax-expression? expression global)
        (and (pair? expression)
             (let ((ope (%lookup-environment (car expression) global '())))
               (and (pair? ope) (eq? 'built-in-define-syntax (car ope))))))

     (define (onif-expand-environment)
        `((syntax-symbol-hash ,(onif-scm-env-tiny-core))))

     (define (onif-expand/make-library-environment)
        `((syntax-symbol-hash ,(onif-scm-env/make-env-for-library))))

     (define (onif-expand/core-library-name? lib-name)
       (equal? lib-name '(onif-lib core)))

     (define (%import-rename env renames)
       (let ((res (hash-table-copy env #t)))
         (let loop ((renames renames))
           (unless (null? renames)
             (let* ((key (caar renames))
                    (new-key (cadar renames))
                    (val (hash-table-ref env key)))
               (hash-table-delete! res key)
               (hash-table-set! res new-key val))))
         res))

     (define (%namespace->export-global namespace)
         (let* ((exports (onif-misc/namespace-assq 'export-symbols namespace '()))
                (syntax-symbol-hash (onif-misc/namespace-assq 'syntax-symbol-hash namespace));;名前変えたい
                )
            (alist->hash-table
               (->> exports
                    (filter (lambda (symbol)
                              (hash-table-contains? syntax-symbol-hash
                                                    symbol)))
                    (map (lambda (symbol)
                           (cons symbol
                                 (hash-table-ref syntax-symbol-hash symbol)))))
               eq?)))

     (define (onif-expand/make-environment lib-names other-namespaces)
       "Make environment from onif libraries such as (onif core) included lib-names"
       (fold (lambda (lib-name env)
               (cond
                  ((onif-expand/core-library-name? lib-name)
                     (hash-table-merge! env (onif-scm-env-tiny-core)))
                  ((and (eq? (car lib-name) 'rename)
                        (onif-expand/core-library-name? (cadr lib-name)))
                     (hash-table-merge! env (%import-rename (onif-scm-env-tiny-core)
                                                           (cddr lib-name))))
                  ((assoc lib-name other-namespaces)
                   => (lambda (namespace)
                        (let ((exported-global (%namespace->export-global namespace)))
                          (hash-table-merge! env exported-global))))
                  (else env)))
              (make-hash-table (make-eq-comparator))
              lib-names))

     (define (onif-expand/make-syntax-object syntax global stack)
       "This code is only supports usage as a rename.
        eg. (define-syntax define2 define)"
        (if (symbol? syntax)
          (%global-lookup syntax global)
          (let ((ope (%lookup-environment (car syntax) global stack)))
            (case (car ope)
               ((built-in-syntax-rules)
                (let* ((have-ellip (symbol? (cadr syntax)))
                       (ellipsis (if have-ellip (cadr exntax) '...))
                       (literals (if have-ellip
                                   (list-ref syntax 2)
                                   (cadr syntax)))
                       (body (if have-ellip (cdddr syntax) (cddr syntax))))
                  (list 'user-syntax
                        (onif-syntax-rules/make-syntax-rules
                           ellipsis literals body
                           global stack))))
               (else (error "MACRO!"))))))

     (define (onif-expand/defined-symbols expressions symbol-hash)
       (filter-map
         (lambda (expression)
           (cond
             ((not-pair? expression) #f)
              ((onif-misc/define-operator?  (car expression) symbol-hash)
               (cadr expression))
             (else #f)))
         expressions))))
