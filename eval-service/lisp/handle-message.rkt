#lang racket
(require "eval.rkt"
         "../../chat/message/main.rkt"
         "../../chat/contact/message-send.rkt")

(provide handle-message)


(define (handle-message event add-message)
  (define source-message (send event get-message))
  (define m-str (string-trim (send source-message content-to-string)))
  (define expr-str #f)
  (define has-quote-reply? #f)
  (cond
    [(regexp-match #rx"^\\s*#rkt.+" m-str)
     (set! expr-str (string-trim (substring m-str 4)))
     (set! has-quote-reply? #t)]
    [(regexp-match #rx"^\\s*!rkt.+" m-str)
     (set! expr-str (string-trim (substring m-str 4)))
     (set! has-quote-reply? #f)]
    [(regexp-match #rx"^\\s*\\(.+\\)" m-str)
     (set! expr-str (string-trim m-str))])
  (if (and expr-str (not (string=? expr-str "")))
      (handle-api-result (eval-program expr-str
                                       "global"
                                       (send event get-sender))
                         event
                         add-message
                         source-message
                         has-quote-reply?)
      #f))


(define (handle-api-result api-result event add-message source-message has-quote-reply?)
  (cond
    [(send api-result ok?)
     (define mcb (new message-chain-builder%))
     (define add-message (create-add-message mcb))

     (define value (send api-result get-value))
     (define output (send api-result get-output))
     (define has-output (not (null? output)))

     (define (convert-message items)
       (for-each
        (λ (item)
          (add-message
           (match (hash-ref item 'type "text")
             ["image" (new image-message% [path (hash-ref item 'path)])]
             [else (new mirai-code-message% [code (string-trim (hash-ref item 'content))])])))
        items))

     (when has-quote-reply?
       (add-message (make-quote-reply source-message)))
     (convert-message output)
     (when (and (not (null? value))
                (or (not has-output)
                    (not (match value
                           [(list (hash-table ('type "text") ('content "#<void>"))) #t]
                           [else #f]))))
       (when has-output
         (add-message "\n"))
       (convert-message value))

     (define message-chain (send mcb build))
     (message-receipt-promise-then
      (send (send event get-subject) send-message message-chain)
      (λ (_)
        (for-each
         (λ (m)
           (when (and (is-a? m image-message%) (send m get-path))
             (delete-file (send m get-path))))
         (send message-chain to-list))))]
    [else
     (define error (send api-result get-error))
     (add-message (face-from-id 270))
     (add-message error)]))