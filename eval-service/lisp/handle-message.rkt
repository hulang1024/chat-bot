#lang racket
(require "eval.rkt"
         "../../chat/message/main.rkt")

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
                         add-message
                         source-message
                         has-quote-reply?)
      #f))


(define (handle-api-result api-result add-message source-message has-quote-reply?)
  (cond
    [(send api-result ok?)
     (define value (send api-result get-value))
     (define output (send api-result get-output))
     (define (convert-message items)
       (for-each
        (λ (item)
          (add-message
           (match (hash-ref item 'type "text")
             ["image" (new image-message% [path (hash-ref item 'path)])]
             [else (new mirai-code-message% [code (string-trim (hash-ref item 'content))])])))
        items))
     
     (define has-output (not (null? output)))
     (when has-quote-reply?
       (add-message (make-quote-reply source-message)))
     (convert-message output)
     (when (and (not (null? value))
                (or (not has-output)
                    (not (match value
                           [(hash-table ('type "text") ('content "#<void>")) #f]
                           [else #f]))))
       (when has-output
         (add-message "\n"))
       (convert-message value))]
    [else
     (define error (send api-result get-error))
     (add-message (face-from-id 270))
     (add-message error)]))