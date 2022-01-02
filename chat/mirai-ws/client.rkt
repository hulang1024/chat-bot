#lang racket

;;; 与mirai-http-api通信的ws客户端

(require racket/match
         net/rfc6455
         net/url
         json
         "heartbeat.rkt"
         "../contact/message-receipt.rkt")

(provide client-connect)


(define (client-connect #:config config
                        #:debug-mode debug-mode
                        #:on-connected on-connected
                        #:on-message-channel-data on-message-channel-data)
  (define url (string->url (format "ws://~a:~a/message"
                                   (hash-ref config 'hostname)
                                   (hash-ref config 'port))))
  (define conn (ws-connect url))
  (let loop ()
    (sync
     (handle-evt
      conn
      (λ (raw)
        (unless (eof-object? raw)
          (when debug-mode
            (printf "<- ~a\n" raw))
          (let* ([raw (string->jsexpr raw)]
                 [syncId (hash-ref raw 'syncId)]
                 [data (hash-ref raw 'data)])
            (cond
              ; 连接成功
              [(and (string=? syncId "")
                    (hash-has-key? data 'session))
               (when (= (hash-ref data 'code) 0)
                 (on-connected conn)
                 (start-heartbeat conn))]
              ; 服务端主动发起的
              [(string=? syncId "-1")
               (on-message-channel-data data)]
              ; 请求响应
              [else
               (message-receipt-promise-resolve syncId data)
               (when debug-mode
                 (match-define (hash-table ('msg msg)) data)
                 (when (non-empty-string? msg)
                   (printf "server msg: ~a\n" msg)))]))
          (loop)))))))