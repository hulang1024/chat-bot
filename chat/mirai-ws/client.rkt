#lang racket

;;; 与mirai-http-api通信的ws客户端

(require net/rfc6455
         net/url
         json)


(provide
 ; 连接
 (rename connect client-connect))


(define (connect #:config config #:on-message-channel-data on-message-channel-data)
  (define url (string->url (format "ws://localhost:~a/message"
                                   (hash-ref config 'port))))
  (define conn (ws-connect url))
  (let loop ()
    (define raw (ws-recv conn))
    (unless (eof-object? raw)
      (printf "raw message: ~a\n" raw)
      (let* ([raw (string->jsexpr raw)]
             [data (hash-ref raw 'data)])
        (on-message-channel-data data))
      (loop))))