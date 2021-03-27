(define-library (kallcc expand preprocess)
  (import (scheme base) (scheme list)
          (onif misc) (scheme write)

          (niyarin thread-syntax)

          (prefix (niyarin preder) p/)
          (prefix (kallcc expand library) ke-library/))
  (export remove-outer-begin separate-library import-separate library-add-import-key
          library-add-export-keys preprocess)
  ;;名前空間に左右されないものの前処理(importやdefine-library)
  (begin
     (define (remove-outer-begin scm-code global) scm-code)

     (define (%define-library-expression? expression)
       (and (pair? expression)
            (eq? (car expression) 'define-library)))

     (define (separate-library scm-code)
       (let-values (((libraries global-code)
                     (partition %define-library-expression?  scm-code)))
         (cons `(() ((body ,global-code)))
               (map (lambda (define-library-code)
                      `(,(cadr define-library-code);name
                           ((body ,(cddr define-library-code)))))
                    libraries))))

     (define (library-add-import-key valid-library)
        (let ((body (ke-library/body valid-library)))
          (let-values (((imports expressions)
                        (partition (p/and pair? (p/comp car
                                                        (p/eq? 'import))) body)))
            (ke-library/updaten valid-library
                                'import-libraries
                                (if (null? imports) '() (cdar imports))

                                'import-name-only
                                (if (null? imports)
                                  '()
                                  (%import-expression->lib-name-only
                                    (cdar imports)))

                                'body expressions))))

     (define (%export-expression->visible-names export-expression)
       (append (remove (p/and pair? (p/comp car (p/eq? 'rename))) export-expression)
               (map cadr (filter (p/and pair?  (p/comp car (p/eq? 'rename))) export-expression))))

     (define (%import-expression->lib-name-only import-expression)
       (map (lambda (import-set)
              (cond
                (((p/comp car
                          (p/or (p/eq? 'only) (p/eq? 'except)
                                (p/eq? 'prefix) (p/eq? 'rename))) import-set)
                 => cadr)
                (else import-set)))
            import-expression))

     (define (library-add-export-keys valid-library)
       (let ((body (ke-library/body valid-library)))
          (let-values (((exports expressions)
                        (partition (p/and pair? (p/comp car
                                                        (p/eq? 'export))) body)))
            (ke-library/updaten valid-library
                                'export (if ((p/not null?) exports) (cdar exports) '())
                                'exported-visible-name
                                (if ((p/not null?) exports)
                                  (%export-expression->visible-names (cdar exports))
                                  '())
                                'body expressions))))

     (define (import-separate scm-code)
       (let loop ((res '())
                  (current '())
                  (code scm-code))
         (cond
           ((null? code) (remove null? (reverse (cons (reverse current) res))))
           ((and (pair? (car code)) (eq? (caar code) 'import))
            (loop (cons (reverse current) res) (list (car code)) (cdr code)))
           (else (loop res (cons (car code) current) (cdr code))))))

    (define (%separate-import librarys)
      (append (map (lambda (code) `(() ((body ,code))))
                     (import-separate (ke-library/body (assq '() librarys))))
              (remove (p/comp car null?)
                      librarys)))

    (define (preprocess code)
      (->> (%separate-import (separate-library code))
           (filter (lambda (x) (not (null? x))))
           (map library-add-import-key)
           (map library-add-export-keys)))))
