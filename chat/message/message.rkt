#lang racket

(provide (all-defined-out))


(define message<%>
  (interface ()
    content-to-string))


(define single-message<%>
  (interface (message<%>)))


(define message-chain%
  (class* object% (message<%>)
    (super-new)

    (define lst null)

    (define/public (get-list) lst)

    (define/public (add m)
      (set! lst (append lst (cons m null))))

    (define/public (empty?) (null? lst))

    (define/public (content-to-string) (void))))


(define source%
  (class* object% (single-message<%>)
    (super-new)

    (init-field id time)

    (define/public (content-to-string) (void))))


(define plain%
  (class* object% (single-message<%>)
    (super-new)

    (init-field text)

    (define/public (content-to-string) text)))


(define at%
  (class* object% (single-message<%>)
    (super-new)

    (init-field target display)

    (define/public (content-to-string) (void))))


(define quote%
  (class* object% (single-message<%>)
    (super-new)

    (init-field id group-id sender-id target-id origin)
    
    (define/public (content-to-string) (void))))