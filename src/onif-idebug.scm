(define-library (onif idebug)
   (import (scheme base) (onif symbol) (scheme cxr) (scheme write) (scheme list))
   (export onif-idebug-icode->code onif-idebug/debug-display onif-idebug/asm-vis )
   (begin
     (define (%opt-ref opt key)
      (cond
        ((assq key opt) => cadr)
        (else #f)))

     (define (%icode->code/meta-lambda-conv icode opt)
      (let ((not-contain-meta-info (%opt-ref opt 'not-contain-meta-info))
            (shaping-meta-info (%opt-ref opt 'shaping-meta-info)))
        (cond
          (not-contain-meta-info
           (%icode->code/expression-conv
             (cons* 'lambda (cadr icode) (cdddr icode))
             opt))
          (shaping-meta-info
            (let loop ((infos (caddr icode))
                       (res '()))
              (if (null? infos)
                `(LAMBDA-META
                   ,(map (lambda (x) (%icode->code x opt))
                         (cadr icode))
                  ,@(append '(|{|) (reverse res) '(|}|))
                  .
                  ,(map (lambda (x) (%icode->code x opt))
                        (cdddr icode)))
                (let ((key (caar infos))
                      (val (cadar infos)))
                  (loop (cdr infos)
                        (cons* "," (%icode->code val opt)
                              (string-append (symbol->string key) ":")
                              res))))))
          (else (map (lambda (x) (%icode->code x opt)) icode)))))

     (define (onif-idebug/debug-display icode . opt)
       (display (apply onif-idebug-icode->code (cons icode opt))))

     (define (%icode->code/expression-conv icode opt)
       (let ((operator
               (if (onif-symbol/onif-symbol? (car icode))
                 (onif-symbol/ref-symbol (car icode))
                 (car icode))))
          (case operator
            ((LAMBDA-META) (%icode->code/meta-lambda-conv icode opt))
            (else (map (lambda (x) (%icode->code x opt)) icode)))))

     (define (%icode->code icode opt)
      (cond
         ((onif-symbol? icode) (onif-symbol->symbol icode))
         ((null? icode) icode)
         ((list? icode) (%icode->code/expression-conv icode opt))
         ((pair? icode)
          (cons (%icode->code (car icode) opt)
                (%icode->code (cdr icode) opt)))
         ((vector? icode)
          (vector-map (lambda (x) (%icode->code x opt)) icode))
         (else icode)))

     (define (onif-idebug-icode->code icode . opt)
       (%icode->code icode opt))))
