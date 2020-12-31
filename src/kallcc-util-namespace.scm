(define-library (kallcc util namespace)
  (import (scheme base) (scheme hash-table) (scheme comparator)
          (prefix (kallcc util namespace) kunamespace/))
  (export merge filter-keys)
  (begin
    (define (merge g1 g2)
      (let ((g (hash-table-copy g1 #t)))
        (hash-table-union! g g2)))

    (define (filter-keys g keys)
      (alist->hash-table (map (lambda (k) (cons k (hash-table-ref g k))) keys)
                         (make-eq-comparator)))))
