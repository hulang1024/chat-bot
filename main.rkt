#lang racket

(require racket/cmdline
         "config.rkt"
         "chat/event/message-event.rkt"
         "chat/bot.rkt"
         "chat/message/main.rkt"
         "eval-service/main.rkt"
         "func-lib/message-handler.rkt")


(define verbose-mode (make-parameter #f))
(define debug-mode (make-parameter #f))

(command-line #:once-each
              [("-v" "--verbose")
               "日志"
               (verbose-mode #t)]
              [("-d" "--debug")
               "调试日志"
               (debug-mode #t)])

(displayln "Started")

(define bot (new bot%
                 [server-config mirai-ws-server-config]
                 [verbose (verbose-mode)]
                 [client-debug (debug-mode)]))

(send bot subscribe-message-event
      (λ (event)
        (define subject (send event get-subject))
        (define mcb (new message-chain-builder%))
        (define add-message (create-add-message mcb))

        (define handled (handle-message bot event add-message))
        (when (not handled)
          (set! handled (handle-eval-service-message event add-message)))
        
        (when handled
          (define mc (send mcb build))
          (when (not (send mc empty?))
            (send subject send-message (send mc trim))))))

(displayln "连接中...")
(send bot login
      (λ ()
        (displayln "已连接到服务器:)")))

(let loop ()
  (unless (equal? (read-line) "")
    (loop)))