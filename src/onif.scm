(include "./onif-phases.scm")
(include "./onif-expand.scm")
(include "./lib/thread-syntax.scm")

(import (scheme base) (scheme write) (scheme read)
        (scheme file) (only (scheme process-context) command-line)
        (niyarin thread-syntax)
        (onif phases) (onif expand))

(define (read-all input-port)
  (let ((res-cell (list #f)))
     (let loop ((cell res-cell))
       (let ((code (read input-port)))
         (unless (eof-object? code)
            (set-cdr! cell (list code))
            (loop (cdr cell)))))
     (cdr res-cell)))

(define (run filename)
  (call-with-input-file filename
      (lambda (input-port)
        (let* ((code (read-all input-port))
               (cps-code (-> code
                            (onif-phases/first-stage (onif-expand-environment))
                            (onif-phases/cps-conv))))
         (display cps-code)))))

(define (onif-main)
  (let ((cmd (command-line)))
    (unless (null? (cdr cmd))
      (run (cadr cmd)))))

(onif-main)
