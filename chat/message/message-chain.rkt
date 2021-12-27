#lang racket
(require "./message.rkt")

(provide message-chain%
         as-message-chain)


(define message-chain%
  (class* object% (message<%>)
    (super-new)

    (define lst null)
    (define content-string null)

    (define/public (to-list) lst)

    (define/public (get p)
      (cond
        [(number? p) (list-ref lst p)]
        [(class? p) (get (is-a?/c p))]
        [(procedure? p)
         (findf p lst)]))

    (define/public (add m)
      (when (string? m)
        (set! m (if (string=? m "") #f (new plain% [text m]))))
      (when m
        (set! lst (append lst (cons m null)))
        (set! content-string null)))

    (define/public (empty?) (null? lst))

    (define/public (trim)
      (define left (if (is-a? (list-ref lst 0) source%)
                       (list-ref lst 1)
                       (list-ref lst 0)))
      (define right (last lst))

      (when (is-a? left plain%)
        (define trimmed (string-trim (send left get-text) #:right? #f))
        (send left set-text trimmed))
      (when (is-a? right plain%)
        (define trimmed (string-trim (send right get-text) #:left? #f))
        (send right set-text trimmed))
      this)

    (define/public (content-to-string)
      (when (null? content-string) 
        (define strs (filter (compose not false?)
                             (map (λ (m) (send m content-to-string)) lst)))
        (set! content-string (string-join strs "")))
      content-string)


(define (as-message-chain message)
  (cond
    [(is-a? message message-chain%)
      message]
    [(or (is-a? message single-message<%>) (string? message))
     (define c (new message-chain%))
     (send c add message)
     c]))