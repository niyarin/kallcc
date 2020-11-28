(include "./onif-phases.scm")
(include "./onif-expand.scm")
(include "./lib/thread-syntax.scm")
(include "./onif-idebug.scm")
(include "./onif-opt-phases.scm")

(import (scheme base) (scheme write) (scheme read) (scheme list)
        (scheme file) (only (scheme process-context) command-line)
        (niyarin thread-syntax)
        (onif idebug)
        (onif phases) (onif opt phases)
        (onif expand))

(define (read-all input-port)
  (let ((res-cell (list #f)))
     (let loop ((cell res-cell))
       (let ((code (read input-port)))
         (unless (eof-object? code)
            (set-cdr! cell (list code))
            (loop (cdr cell)))))
     (cdr res-cell)))

(define (%run-run filename)
   (call-with-input-file filename
      (lambda (input-port)
        (let* ((ref-libs (%load-ref-lib))
               (code (append ref-libs
                             (read-all input-port)))
               (asm (-> code
                        (onif-phases/first-stage (onif-expand-environment))
                        onif-phases/cps-conv
                        ;onif-phase/meta-lambda
                        ;onif-opt-phases/remove-unnecesarry-define
                        ;onif-phase/make-name-local-ids
                        ;onif-phase/flat-lambda
                        ;onif-phase/new-asm
                        )))
         (newline)
         (display "RES=")(newline)
         (onif-idebug/debug-display asm)))))


(define *reflib-dir* "../ref-liv/")
(define (%load-ref-lib)
  (append-map (lambda (name)
                (call-with-input-file (string-append *reflib-dir* name)
                    (lambda (input-port)
                      (read-all input-port))))
                '("pair.scm" "syntax1.scm")))

(define (%run-simple-expand filename)
   (call-with-input-file filename
      (lambda (input-port)
        (let* ((ref-libs (%load-ref-lib))
               (code (append ref-libs
                             (read-all input-port)))
               (expand-code (-> code
                                (onif-phases/first-stage (onif-expand-environment)))))
          (->> (cadr (assq 'body (cadr (assq '() expand-code))))
               (for-each (lambda (x)
                           (onif-idebug/debug-display x)
                           (newline))))))))

(define (run cmd filename)
  (cond
    ((eq? cmd 'run) (%run-run filename))
    ((eq? cmd 'expand) (%run-simple-expand filename))))

(define (%simple-opt-parse inputs)
  (let loop ((inputs inputs)
             (cmd #f)
             (files '()))
    (cond
      ((null? inputs) (list cmd files))
      ((string=? (car inputs) "--command")
       (loop (cddr inputs) (string->symbol (cadr inputs)) files))
      (else (loop (cdr inputs) cmd (cons (car inputs) files))))))

(define (onif-main)
  (let ((cmd (command-line)))
    (unless (null? (cdr cmd))
      (let* ((options (%simple-opt-parse cmd))
             (cmd (if (not (car options)) 'run (car options)))
             (file (caadr options)))
         (run cmd file)))))

(onif-main)
