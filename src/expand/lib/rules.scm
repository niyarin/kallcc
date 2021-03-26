(define-library (niyarin rules)
   (import (scheme base) (scheme case-lambda))
   (export rules/match rules/expand rules/match-expand)
   (begin
      (define (%parse-list ellipsis-symbol ls)
        (let loop ((ls ls)
                   (type 'list)
                   (head '())
                   (tail '())
                   (ellipsis #f))
          (cond
            ((null? ls)
             `((type ,type)
               (head ,(reverse head))
               (tail ,(reverse tail))
               (last-cdr '())
               (ellipsis ,ellipsis)))
            ((not (pair? ls))
             `((type ,(if (eq? type 'list)
                        'improper-list
                        'improper-ellipsis-list))
               (head ,(reverse head))
               (tail ,(reverse tail))
               (last-cdr ,ls)
               (ellipsis ,ellipsis)))
            ((and (pair? (cdr ls))
                  (eq? (cadr ls) ellipsis-symbol))
             (loop (cddr ls)
                   'ellipsis
                   head
                   tail
                   (car ls)))
            ((eq? type 'list)
             (loop (cdr ls)
                   type
                   (cons (car ls) head)
                   tail
                   ellipsis))
            (else
              (loop (cdr ls)
                    type
                    head
                    (cons (car ls) tail)
                    ellipsis)))))

      (define (proper-list-last-cdr-pair improper-list)
        (let ((plist-box (list #f)))
          (let loop ((ls improper-list)
                     (cell plist-box))
            (cond
              ((not (pair? ls))
               (cons (cdr plist-box) ls))
              (else
                (set-cdr! cell (list (car ls)))
                (loop (cdr ls)
                      (cdr cell)))))))

      (define (%append-res res1 res2)
        (append
         (map (lambda (apair2)
                (let* ((k (car apair2))
                       (v (cdr apair2))
                       (apair1 (assq k res1)))
                  (if apair1
                    (cons k (append (cdr apair1) (list v)))
                    (cons k (list v)))))
              res2)
         res1))

      (define (%alists-distinct . args)
        (let ((alist (apply append args)))
          (let loop ((als alist)
                     (res '())
                     (keys '()))
            (cond
              ((null? als) res)
              ((memq (caar als) keys)
               (loop (cdr als) res keys))
              (else
                (loop (cdr als)
                      (cons (car als) res)
                      (cons (caar als) keys)))))))

      (define (%remove-ellipsis-type type)
        (case type
           ((improper-ellipsis-list) 'improper-list)
           ((ellipsis) 'list)
           (else type)))

      (define (%match-list ellipsis-symbol literals rule input-list break)
        (let* ((_input (proper-list-last-cdr-pair input-list))
               (input-last-cdr (cdr _input))
               (parsed-list (%parse-list ellipsis-symbol rule))
               (type (cadr (assq 'type parsed-list)))
               (head (cadr (assq 'head parsed-list)))
               (tail (cadr (assq 'tail parsed-list)))
               (last-cdr (cadr (assq 'last-cdr parsed-list)))
               (ellipsis (cadr (assq 'ellipsis parsed-list))))
           (let loop ((type type)
                      (input (car _input))
                      (head head)
                      (res '()))
             (cond
               ((not (null? head))
                (loop type
                      (cdr input)
                      (cdr head)
                      (append (%match ellipsis-symbol literals (car head) (car input) break) res)))
              ((and (null? head)
                    (or (eq? type 'ellipsis) (eq? type  'improper-ellipsis-list)))
               (let ((loop-cnt (- (length input) (length tail))))
                 (when (negative? loop-cnt) (break #f))
                 (let i-loop ((i loop-cnt)
                              (input input)
                              (res res))
                   (cond
                     ((zero? i) (loop (%remove-ellipsis-type type) input tail res))
                     (else
                       (i-loop (- i 1)
                               (cdr input)
                               (%alists-distinct (%append-res res
                                                              (%match ellipsis-symbol
                                                                      literals
                                                                      ellipsis
                                                                      (car input)
                                                                      break)))))))))
              ((and (not (null? input-last-cdr ))
                    (memq type '(improper-list improper-ellipsis-list)))
               (append
                 (%match ellipsis-symbol literals last-cdr input-last-cdr break)
                 res))
              ((eq? type 'improper-list)
               (append
                 (%match ellipsis-symbol literals last-cdr input break)
                 res))
              ((and (eq? type 'list) (not (null? input)))
                (break #f))
              (else res)))))


      (define (%match ellipsis literals rule input break)
        (cond
          ((and (pair? rule) (pair? input))
           (%match-list ellipsis literals rule input break))
          ((pair? rule) (break #f))
          ((and (vector? rule) (vector? input))
           (%match-list ellipsis literals (vector->list rule) (vector->list input) break))
          ((vector? rule) (break #f))
          ((and (symbol? rule)
                (not (memq rule literals)))
           (list (cons rule input)))
          ((equal? rule input) '())
          (else (break #f))))

      (define (%match-boot ellipsis literal rule input)
        (call/cc
          (lambda (break)
            (%match ellipsis literal rule input break))))

      (define (%tree-ref tree refs break . debug-info)
         (let loop ((refs refs)
                    (tree tree))
           (cond
             ((null? refs) tree)
             ((not (list?  tree) )(error "Wrong depth to expand." debug-info))
             ((<= (length tree) (car refs))
              (break (length refs)))
             (else
               (loop (cdr refs) (list-ref tree (car refs)))))))

      (define (%expand-pair ellipsis template alist refs break)
        (let ((res-cell-top (list #f)))
            (let loop ((ls template)
                       (res-cell res-cell-top))
              (cond
                ((not (pair? ls))
                 (set-cdr! res-cell (%expand ellipsis ls alist refs break))
                 (cdr res-cell-top))
                ((and (pair? (cdr ls))
                      (eq? (cadr ls) ellipsis))
                    (let _loop ((i 0) (res-cell res-cell))
                      (let ((ok (call/cc
                                  (lambda (_break)
                                     (list (%expand ellipsis (car ls)
                                                    alist (cons i refs)
                                                    _break))))))
                        (cond
                          ((and (pair? ok) ;;not matched symbol ...
                                (eq? (car ok) (car ls)))
                           (loop (cddr ls) res-cell))
                          ((not (integer? ok))
                            (set-cdr! res-cell ok)
                            (_loop (+ i 1) (cdr res-cell)))
                          ((zero? (- ok 1)) (loop (cddr ls) res-cell))
                          (else (break (- ok 1)))))))
                (else
                  (set-cdr! res-cell
                            (list (%expand ellipsis (car ls)
                                           alist refs break)))
                  (loop (cdr ls) (cdr res-cell)))))))


      (define (%expand ellipsis template alist refs break)
        (cond
          ((pair? template)
           (%expand-pair ellipsis template alist refs break))
          ((and (symbol? template)
                (assq template alist))
           => (lambda (apair)
                (%tree-ref (cdr apair) (reverse refs) break template)))
          (else template)))


      (define (%expand-boot ellipsis template alist)
          (call/cc
            (lambda (break)
                (%expand ellipsis template alist '() break))))

      (define rules/match
        (case-lambda
          ((rule input)
           (%match-boot '... '() rule input))
          ((literals rule input)
           (%match-boot '... literals rule input))
          ((ellipsis literals rule input)
           (%match-boot ellipsis literals rule input))))

      (define rules/expand
        (case-lambda
          ((template alist)
           (%expand-boot '... template alist))
          ((ellipsis template alist)
           (%expand-boot ellipsis template alist))))

      (define (%match-expand-boot ellipsis literal rule template input)
        (let ((match-res (rules/match ellipsis literal rule input)))
          (and match-res
              (rules/expand ellipsis template match-res))))

      (define rules/match-expand
        (case-lambda
          ((ellipsis literals rule template input)
           (%match-expand-boot ellipsis literals rule template input))
          ((literals rule template input)
           (%match-expand-boot '... literals rule template input))
          ((rule template input)
           (%match-expand-boot '... '() rule template input))))))
