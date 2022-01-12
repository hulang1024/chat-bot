#lang racket
(require "eval.rkt"
         "../../chat/message/main.rkt"
         "../../chat/contact/message-send.rkt")

(provide handle-message
         execute-program)


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
      (execute-program event add-message expr-str has-quote-reply?)
      #f))


(define (execute-program event add-message expr has-quote-reply? [quiet-fail? #f])
  (handle-api-result (eval-program expr
                                   "global"
                                   (send event get-sender))
                     event
                     add-message
                     has-quote-reply?
                     quiet-fail?))


(define (handle-api-result api-result event add-message has-quote-reply? quiet-fail?)
  (define ok? (send api-result ok?))
  (define value (send api-result get-value))
  (define output (send api-result get-output))
  (define length-good? (if ok? (output-length-good? (append value output)) #f))
  
  (cond
    [(and ok? length-good?)
     (define mcb (new message-chain-builder%))
     (define add-message (create-add-message mcb))
     (define (convert-message items)
       (for-each
        (λ (item)
          (add-message
           (match (hash-ref item 'type "text")
             ["image" (new image-message%
                           [path (hash-ref item 'path)]
                           [url (hash-ref item 'url)])]
             ["audio" (new voice-message%
                           [path (hash-ref item 'path)]
                           [url (hash-ref item 'url)])]
             [else (new mirai-code-message% [code (hash-ref item 'content)])])))
        items))
     (define has-output (not (null? output)))
     (when has-quote-reply?
       (add-message (make-quote-reply (send event get-message))))
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
      (send (send event get-subject) send-message (send message-chain trim))
      (λ (_)
        (for-each
         (λ (m)
           (when (and (or (is-a? m image-message%) (is-a? m voice-message%))
                      (path-string? (send m get-path)))
             (delete-file (send m get-path))))
         (send message-chain to-list))))]
    [(and ok? (not length-good?))
     (add-message "消息太长啦")]
    [quiet-fail? #f]
    [else
     (define error (send api-result get-error))
     (add-message "🎈")
     (add-message error)]))

(define (output-length-good? output)
  (define text-line-max 61)
  (define image-max 11)
  (define audio-max 5)

  (define text-line-count 0)
  (define image-count 0)
  (define audio-count 0)
  (for-each
   (λ (item)
     (match (hash-ref item 'type "text")
       ["text"
        (define content (hash-ref item 'content))
        (set! text-line-count (+ text-line-count (length (string-split content "\n"))))]
       ["image"
        (set! image-count (+ image-count 1))]
       ["audio"
        (set! audio-count (+ audio-count 1))]))
   output)
  (and (< text-line-count text-line-max)
       (< image-count image-max)
       (< audio-count audio-max)))