#lang racket

(provide make-command-response-promise
         command-response-promise-resolve
         command-response%)


(define promise%
  (class object%
    (super-new)

    (define then-procs null)
    (define resolve? #f)
    (define result (void))

    (define/public (then proc)
      (if resolve?
          (proc result)
          (set! then-procs (append then-procs (cons proc null)))))

    (define/public (resolve result)
      (when (not resolve?)
        (set! resolve? #t)
        (for-each (λ (proc)
                    (set! result (proc result)))
                  then-procs)
        (set! then-procs null)))))
    
(define promises (make-hash))

(define (make-command-response-promise id)
  (define promise (new promise%))
  (hash-set! promises id promise)
  promise)

(define (command-response-promise-resolve id response)
  (when (hash-has-key? promises id)
    (define promise (hash-ref promises id))
    (send promise resolve response)
    (hash-remove! promises id)))


(define command-response%
  (class object%
    (super-new)

    (init-field code msg)

    (define/public (get-code) code)
    (define/public (get-msg) msg)
    (define/public (ok?) (= code 0))))