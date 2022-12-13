#lang racket
(require math/base
         net/url
         json
         "../config.rkt"
         "../chat/message/main.rkt")

(provide reply-talk
         (prefix-out reply: is-time?)
         (prefix-out reply: active-mode)
         (prefix-out reply: set-active-mode))

(define active-mode "normal")
(define last-reply-time 0)

(define (reply-talk msg add-message)
  (set! last-reply-time (current-inexact-milliseconds))
  (with-handlers ([(const #t) (λ (v) #f)])
    (define-values (status headers in)
      (http-sendrecv/url
       (string->url (format "http://127.0.0.1:1880/chat?text=~A"
                            msg))))
    (define api-result (string->jsexpr (port->string in)))
    (define str (hash-ref api-result 'content))
    (add-message (new mirai-code-message% [code str]))))

(define (is-time?)
  (define max
    (case active-mode
      [("reply") 1]
      [("active") 5]
      [("normal") 20]
      [("quiet") 0]))
  (and (not (= max 0))
       (> (- (current-inexact-milliseconds) last-reply-time) 1000)
       (or (= max 1) (= (random-integer 0 max) 1))))

(define (set-active-mode mode)
  (set! active-mode mode))