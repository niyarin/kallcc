(define-library (topological-sort)
   (import (scheme base))
   (export topological-sort)
   (begin
      (define (%make-indegree-vector adjacency-vector)
        (let ((res (make-vector (vector-length adjacency-vector) 0)))
          (vector-for-each
            (lambda (vertexes)
              (for-each
                (lambda (vertex)
                  (vector-set! res vertex (+ (vector-ref res vertex) 1)))
                vertexes))
            adjacency-vector)
          res))

      (define-record-type <QUE>
            (%que start end vec)
            %que?
            (start %start-ref %start-set!)
            (end %end-ref %end-set!)
            (vec %vec-ref %vec-set!))

      (define (%que-push! que object)
        (when (= (%end-ref que) (vector-length (%vec-ref que)))
            (let ((new-vector (make-vector (* (vector-length (%vec-ref que)) 2)))
                  (end (%end-ref que))
                  (start (%start-ref que)))
              (%end-set! que (- end start))
              (%start-set! que 0)
              (vector-copy! new-vector 0 (%vec-ref que) start end)
              (%vec-set! que new-vector)))
        (vector-set! (%vec-ref que) (%end-ref que) object)
        (%end-set! que (+ (%end-ref que) 1)))

      (define (%que-pop! que)
        (let ((res (vector-ref (%vec-ref que) (%start-ref que))))
         (%start-set! que (+ (%start-ref que) 1))
         res))

      (define (%make-que)
         (%que 0 0 (make-vector 2)))

      (define (%que-empty? que)
        (= (%end-ref que) (%start-ref que)))

      (define (topological-sort adjacency-vector)
        (let* ((indegree-vector (%make-indegree-vector adjacency-vector))
               (q (%make-que))
               (res '()))
            (let loop ((i 0))
              (unless (= i (vector-length indegree-vector))
                  (when (zero? (vector-ref indegree-vector i))
                    (%que-push! q i))
                  (loop (+ i 1))))
            (let loop ()
              (unless (%que-empty? q)
                  (let ((v (%que-pop! q)))
                    (set! res (cons v res))
                    (for-each
                      (lambda (to-v)
                        (when (= (vector-ref indegree-vector to-v) 1)
                           (%que-push! q to-v))
                        (vector-set! indegree-vector
                                     to-v
                                     (- (vector-ref indegree-vector to-v) 1)))
                      (vector-ref adjacency-vector v)))
                  (loop)))
            res))))
