#lang racket
(require "./message.rkt")

(provide message-chain%
         as-message-chain)


(define message-chain%
  (class* object% (message<%>)
    (super-new)

    (define messages null)
    (define content-string null)

    (define/public (to-list) messages)

    (define/public (get p)
      (cond
        [(number? p) (list-ref messages p)]
        [(class? p) (get (is-a?/c p))]
        [(procedure? p)
         (findf p messages)]))

    (define/public (add new-m)
      (define to-add?
        (cond
          [(null? messages) #t]
          [else
           (cond
             [(or (string? new-m)
                  (is-a? new-m plain%)
                  (is-a? new-m mirai-code-message%))
              ; 将新文本合并到上个文本或code（也是一种文本)消息里
              (define pre-m (last messages))
              (cond
                [(or (is-a? pre-m plain%)
                     (is-a? pre-m mirai-code-message%))
                 (define last-text (send pre-m get-text))
                 (define new-text (if (string? new-m) new-m (send new-m get-text)))
                 (send pre-m set-text (string-append last-text new-text))
                 #f]
                [else #t])]
             [else #t])]))
         
      (when to-add?
        (set! new-m (if (string? new-m)
                        (new plain% [text new-m])
                        new-m))
        (set! messages (append messages (cons new-m null)))
        (set! content-string null)))

    (define/public (empty?) (null? messages))

    (define/public (trim)
      (define left (if (is-a? (list-ref messages 0) source%)
                       (list-ref messages 1)
                       (list-ref messages 0)))
      (define right (last messages))

      (when (or (is-a? left plain%) (is-a? left mirai-code-message%))
        (define trimmed (string-trim (send left get-text) #:right? #f))
        (send left set-text trimmed))
      (when (or (is-a? right plain%) (is-a? right mirai-code-message%))
        (define trimmed (string-trim (send right get-text) #:left? #f))
        (send right set-text trimmed))
      this)

    (define/public (content-to-string)
      (when (null? content-string) 
        (define strs (filter (compose not false?)
                             (map (λ (m) (send m content-to-string)) messages)))
        (set! content-string (string-join strs "")))
      content-string)))


(define (as-message-chain message)
  (cond
    [(is-a? message message-chain%)
      message]
    [(or (is-a? message single-message<%>) (string? message))
     (define c (new message-chain%))
     (send c add message)
     c]))