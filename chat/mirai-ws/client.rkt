#lang racket

;;; 与mirai-http-api通信的ws客户端

(require racket/match
         net/rfc6455
         net/url
         json)


(provide client-connect
         client-send-command)

(define (client-connect #:config config
                        #:debug-mode debug-mode
                        #:on-connected on-connected
                        #:on-message-channel-data on-message-channel-data)
  (define url (string->url (format "ws://localhost:~a/message"
                                   (hash-ref config 'port))))
  (define conn (ws-connect url))
  (let loop ()
    (define raw (ws-recv conn))
    (unless (eof-object? raw)
      (when debug-mode
        (printf "<- ~a\n" raw))
      (let* ([raw (string->jsexpr raw)]
             [syncId (hash-ref raw 'syncId)]
             [data (hash-ref raw 'data)])
        (cond
          ; 连接成功
          [(string=? syncId "")
           (when (= (hash-ref data 'code) 0)
             (on-connected conn))]
          ; 服务端主动发起的
          [(string=? syncId "-1")
           (on-message-channel-data data)]
          ; 请求响应
          [debug-mode
           (match-define (hash-table ('msg msg)) data)
           (printf "server msg: ~a\n" msg)]))
      (loop))))

(define (client-send-command conn send-data)
  (define command (hash-ref send-data 'command))
  (hash-set! send-data 'syncId (format "~a-~a" command (new-sync-id command)))
  (define json (jsexpr->string send-data))
  (ws-send! conn json)
  json)


(define command-sync-ids (make-hash))

(define (new-sync-id command)
  (define sync-id (+ (hash-ref! command-sync-ids command 0) 1))
  (hash-set! command-sync-ids command sync-id)
  sync-id)
