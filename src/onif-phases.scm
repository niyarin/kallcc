(define-library (onif phases)
   (import (scheme base) (scheme list) (scheme cxr) (srfi 69);scheme hash
           (scheme write);
           (only (niyarin thread-syntax) ->> ->)
           (scmspec core) (onif my-specs)
           (onif idebug) (onif symbol) (onif scm env)
           (onif name-ids) (onif alpha conv) (onif flat-lambda) (onif misc)
           (onif new-asm) (onif expand) (onif cps) (onif opt names)
           (prefix (kallcc namespace) knamespace/)
           (prefix (kallcc expand preprocess) kepreprocess/)
           (prefix (kallcc regflat) kregflat/)
           (prefix (kallcc misc) kmisc/)
           (prefix (kallcc meta-lambda) kmlambda/)
           (prefix (kallcc additional-tags) katags/))
   (export onif-phases/pre-expand onif-phases/expand-namespaces
           onif-phases/solve-imported-name onif-phases/solve-imported-symbol
           onif-phases/alpha-conv! onif-phases/first-stage
           onif-phase/meta-lambda onif-phase/make-name-local-ids
           onif-phase/flat-lambda onif-phases/cps-conv onif-phase/asm
           onif-phase/new-asm
           onif-phase/new-asm&global
           onif-phases/add-additional-tags
           onif-phase/regflat)
   (begin
     (define (onif-phases/pre-expand code global)
       "Removes global begin and splites source code based on namespace."
       (scmspec/lcheck
         ((input (code list?)
                 (global hash-table?))
          (output (scmspec/list-of knamespace/namespace?)))
          (let* ((expression-begin-removed
                   (append-map
                     (lambda (expression)
                       (kepreprocess/remove-outer-begin expression global))
                     code)))
             (onif-expand/separate-namespaces expression-begin-removed global))))

     (define (%sort-libraries namespaces global)
       ;;ここのglobalはほんとは正しくない(importが(scheme base)のimportじゃない場合等でこの手続きは正しく動作しない)
       (let* ((libname-imported-libnames-list
                (->> namespaces
                     (map (lambda (namespace)
                            (->> (cadr namespace)
                                (filter (lambda (expression)
                                            (onif-expand/import-expression?
                                              expression
                                              global)))
                                 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(rename ...)とかの処理が未実装
                                 (map cdr)
                                 concatenate
                                 delete-duplicates
                                 (cons (car namespace)))))))
              (libname-table
                (->> (concatenate libname-imported-libnames-list)
                     delete-duplicates
                     (onif-misc/map-indexed xcons))))
         (->> libname-imported-libnames-list
              (map (lambda (namespace)
                     (map (lambda (libname)
                            (cdr (assoc libname libname-table)))
                          namespace))))))

      (define (%expand-loop expressions global expand-environment other-namespaces)
        "Return pair (expand-environment . expressions)."
        (scmspec/lcheck ((input (expressions (scmspec/pair
                                                (scmspec/or my-specs/import-expression?
                                                            my-specs/scheme-expression?)
                                                (scmspec/list-of (scmspec/not my-specs/import-expression?))))
                                (global hash-table?)))
           ;TODO: import macros from other namespaces
           (let ((res (onif-misc/ft-pair)))
              (let loop ((expressions expressions)
                         (global global)
                         (expand-environment expand-environment))
                (cond
                  ((null? expressions)
                   (cons expand-environment (onif-misc/ft-pair-res res)))
                  ((onif-expand/import-expression?  (car expressions) global)
                   (let* ((new-global
                            (onif-expand/make-environment (cdar expressions) other-namespaces))
                          (new-expand-environment
                            `((syntax-symbol-hash ,new-global)
                              (original-syntax-symbol-hash ,(onif-scm-env-tiny-core))
                              (import-libraries
                                ,(remove onif-expand/core-library-name?
                                         (cdar expressions))))))
                      (loop (cdr expressions) new-global new-expand-environment)))
                  ((onif-expand/export-expression? (car expressions) global)
                      (loop (cdr expressions);TODO: Support rename
                            global
                           (cons `(export-symbols ,(cdar expressions))
                                  expand-environment)))
                  (else
                    (onif-misc/ft-pair-push!
                      res
                      (onif-expand (car expressions) global '() expand-environment))
                    (loop (cdr expressions) global expand-environment)))))))

      (define (onif-phases/expand-namespaces this-expressions
                                             namespaces expand-environment)
        ;;たぶん、namespacesは依存関係でトポロジカルソートされている必要がある。
        (->> (append namespaces (list `(() ((body ,this-expressions)))))
             (fold (lambda (namespace res)
                     ;;各ライブラリの中身はpre-expandされていないので、ここで適用
                     (let* ((global (cadr (assq 'syntax-symbol-hash expand-environment)))
                            (pre-expanded-expressions
                                 ;(() code) (<libname> code) ... )
                                 (onif-phases/pre-expand (knamespace/body namespace)
                                                         global))
                            (target-namespace (car pre-expanded-expressions)))
                        (cons (cons (car namespace)
                                    (%expand-loop (knamespace/body target-namespace)
                                                  global expand-environment res))
                              res)))
                   '())))

      (define (onif-phases/alpha-conv! expanded-namespaces symbol-hash)
        (let* ((defined-symbols
                   (->> expanded-namespaces
                        (map (lambda (namespace-expression)
                               (->> (onif-expand/defined-symbols (cddr namespace-expression) symbol-hash)
                                    (map (lambda (sym)
                                           (cons sym (onif-symbol sym))))))))))
          (for-each (lambda (expressions ;namespace
                              renamed-targets)
                      (begin
                         ;;add export-rename info to namespace.
                         (set-car! (cdr expressions)
                                   (cons `(export-rename ,renamed-targets)
                                         (cadr expressions)))
                         (->> (cddr expressions);expressions
                              (for-each
                                (lambda (expression)
                                   (onif-alpha/conv!
                                     expression
                                     (->> (assq 'syntax-symbol-hash (cadr expressions))
                                          cadr)
                                     (list renamed-targets)))))))
               expanded-namespaces
               defined-symbols)))

      (define %namespace-assq knamespace/nassq)

      (define (onif-phases/solve-imported-symbol namespaces)
        "Return alist for import symbols. ((symbol rename-onif-symbol) ...)"
         (map (lambda (namespace)
                (append-map
                  (lambda (libname)
                    (cond
                      ((assoc libname namespaces)
                       => (lambda (lib) (knamespace/nassq 'export-rename lib)))
                      (else (kmisc/kerror "No library." libname))))
                  (knamespace/nassq 'import-libraries namespace)))
              namespaces))

      (define (onif-phases/first-stage code expand-environment)
         ;;expand-enviuronment:((syntax-symbol-hash ,(onif-scm-env-tiny-core)))
         (let* ((global
                  (cadr (assq 'syntax-symbol-hash expand-environment)))
                (namespaces ;knamespace
                  (onif-phases/pre-expand code (onif-scm-env-tiny-core)))
                (expanded-namespaces
                  (onif-phases/expand-namespaces
                    (knamespace/body (car namespaces))
                    (cdr namespaces);ライブラリの名前空間
                    expand-environment))
                (alpha-converted-code
                  (begin (onif-phases/alpha-conv! expanded-namespaces global)
                         expanded-namespaces))
               (namespaces
                   (->> alpha-converted-code
                     (map (lambda (namespace)
                            (list
                              (car namespace)
                              (cons* `(body ,(cddr namespace))
                                     `(name ,(car namespace))
                                      (cadr namespace))))))))
          (for-each
            (lambda (namespace rename-alist)
                (for-each (lambda (expression)
                              (onif-alpha/simple-conv!
                                expression
                                rename-alist
                                global))
                          (knamespace/body namespace)))
                   namespaces
                   (onif-phases/solve-imported-symbol namespaces))
          namespaces))

      (define (onif-phases/cps-conv namespaces)
        (map (lambda (namespace)
               (knamespace/apply-body
                 namespace
                 (lambda (ns-body)
                   (map (lambda (expression)
                          (onif-cps-conv
                             expression
                            (%namespace-assq 'syntax-symbol-hash namespace)))
                        ns-body))))
             namespaces))

      (define (onif-phase/meta-lambda namespaces)
        (map (lambda (namespace)
              (->> (%namespace-assq 'body namespace)
                   (map (lambda (expression)
                            (kmlambda/meta-lambda-conv
                                expression
                                (%namespace-assq
                                  'syntax-symbol-hash
                                  namespace)
                                (cadr namespace))))
                   ((lambda (body)
                       (knamespace/update namespace
                                          'body
                                          body)))))
             namespaces))

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
                      (->> (knamespace/body namespace)
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
                              (let ((lambdas (append-map cadr expression-offsets)))
                                 (onif-misc/ft-pair-push!
                                   res
                                   `(,(car namespace)
                                      ((body ,(map car expression-offsets))
                                       (id-lambdas ,lambdas)
                                       .
                                       ,(cadr namespace))))

                                 (+ (length lambdas) offset)))))))
             0 namespaces)
          (onif-misc/ft-pair-res res)))

      (define (%new-asm-funs namespaces global-ids-box jump-box)
        (->> namespaces
             (map (lambda (namespace)
                    (->> (%namespace-assq 'id-lambdas namespace)
                         ((lambda (id-lambdas)
                              (onif-new-asm/asm-fun
                                       id-lambdas '(1) 0 '() '();body use-registers offset lock local-register
                                       '()
                                       global-ids-box
                                       jump-box
                                       (%namespace-assq 'syntax-symbol-hash
                                                        namespace)))))))
             concatenate
             onif-new-asm/tune))

      (define (%new-asm-body namespaces global-ids-box jump-box)
        (->> namespaces
              (map (lambda (namespace)
                     (->> (reverse (%namespace-assq 'body namespace))
                          (append-map
                            (lambda (body)
                              (if (boolean? body)
                                '()
                                (cons
                                  '(BODY-START)
                                  (onif-new-asm/convert
                                       body '(()) 0 '() '();body use-registers offset lock local-register
                                       '()
                                       global-ids-box
                                       jump-box
                                       (%namespace-assq 'syntax-symbol-hash
                                                        namespace))))))
                          onif-new-asm/tune)))))

       (define (onif-phase/new-asm namespaces)
           (let* ((rnamespaces (reverse namespaces))
                  (global-ids-box (onif-new-asm/make-global-ids-box))
                  (jump-box (list 0))
                  (bodies (%new-asm-body rnamespaces global-ids-box jump-box))
                  (_ (begin (onif-idebug/debug-display (concatenate bodies))(newline)))
                  (funs (%new-asm-funs rnamespaces global-ids-box jump-box)))
               (append
                 funs
                 (concatenate bodies)
                 '((BODY-START) (HALT)))))

       (define (onif-phase/new-asm&global namespaces)
           (let* ((rnamespaces (reverse namespaces))
                  (global-ids-box (onif-new-asm/make-global-ids-box))
                  (jump-box (list 0))
                  (bodies  (remove (lambda (x) (eq? (car x) 'CONCATENATE-BODY))
                                   (onif-new-asm/tune (concatenate (%new-asm-body rnamespaces global-ids-box jump-box)))))
                  (_ (begin (onif-idebug/debug-display bodies)(newline)))
                  (funs (%new-asm-funs rnamespaces global-ids-box jump-box)))
            (values
               (append
                 funs
                 bodies
                 '((BODY-START) (HALT)))
               (car global-ids-box))))

       (define (onif-phase/regflat namespaces)
         (map (lambda (namespace)
                (knamespace/apply-body
                  namespace
                  (lambda (body)
                    (let ((symbol-env (%namespace-assq 'syntax-symbol-hash namespace)))
                    (map (lambda (x) (kregflat/regflat x symbol-env)) body)))))
              namespaces))


       (define (onif-phases/add-additional-tags namespaces)
         (map (lambda (namespace)
                (knamespace/apply-body
                  namespace
                  (lambda (body)
                    (map (lambda (expression)
                           (katags/add-simple-procedure-tag expression (%namespace-assq 'syntax-symbol-hash namespace)))
                         body))))
              namespaces))
       ))
