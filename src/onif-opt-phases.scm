(define-library (onif opt phases)
   (import (scheme base) (onif opt names)
           (onif idebug) (scheme write)
           (scheme list)
           (only (niyarin thread-syntax) ->> ->))
   (export onif-opt-phases/remove-unnecesarry-define)
   (begin

     ;;copy from onif-phases
    (define (%namespace-assq key namespace . default)
        (cond
          ((assq key (cadr namespace)) => cadr)
          ((not (null? default)) (car default))
          (else (error "Namespace doesn't have key." key namespace))))

     (define (%find-vars-in-namespace namespace)
       (append-map
        (lambda (expression)
           (onif-opt-names/calc-use-names
             expression
             (%namespace-assq 'syntax-symbol-hash namespace)))
        (%namespace-assq 'body namespace)))

     (define (%calc-use-vars namespaces)
         (->> namespaces
              (append-map (lambda (namespace) (%find-vars-in-namespace namespace)))
              delete-duplicates))

     (define (onif-opt-phases/remove-unnecesarry-define namespaces)
       (let ((use-vars (%calc-use-vars namespaces)))
          (map (lambda (namespace)
                   (let ((symbol-hash
                            (%namespace-assq 'syntax-symbol-hash
                                             namespace)))
                     (->> (%namespace-assq 'body namespace)
                          (map (lambda (expression)
                                 (onif-ope-names/remove-unnecessary-define
                                   expression use-vars symbol-hash)))
                          ((lambda (expressions)
                             (list (car namespace)
                                   (cons (list 'body (remove null? expressions))
                                      (cadr namespace))))))))
                   namespaces)))))
