#lang racket

(provide make-message-receipt-promise
         message-receipt-promise-then
         message-receipt-promise-resolve)


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
    
(define (message-receipt-promise-then receipt-promise proc)
  (send receipt-promise then proc))

(define promises (make-hash))

(define (make-message-receipt-promise id)
  (define promise (new promise%))
  (hash-set! promises id promise)
  promise)

(define (message-receipt-promise-resolve id result)
  (when (hash-has-key? promises id)
    (define promise (hash-ref promises id))
    (send promise resolve result)
    (hash-remove! promises id)))