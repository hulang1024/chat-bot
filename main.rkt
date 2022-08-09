#lang racket

(require racket/cmdline
         "config.rkt"
         "chat/event/message-event.rkt"
         "chat/bot.rkt"
         "chat/message/main.rkt"
         "eval-service/main.rkt"
         "func-lib/message-handler.rkt"
         "func-lib/login-handler.rkt"
         "nlp/dict.rkt")


(define verbose-mode (make-parameter #f))
(define debug-mode (make-parameter #f))
(define notify-mode (make-parameter #f))

(command-line #:once-each
              [("-v" "--verbose")
               "日志"
               (verbose-mode #t)]
              [("-d" "--debug")
               "调试日志"
               (debug-mode #t)]
              [("--notify")
               "登录成功通知管理员"
               (notify-mode "admin")]
              [("--notify-all")
               "登录成功通知所有群"
               (notify-mode "all")])

(displayln "Started")

(define bot (new bot%
                 [id bot-id]
                 [nickname bot-nickname]
                 [server-config mirai-ws-server-config]
                 [verbose (verbose-mode)]
                 [client-debug (debug-mode)]))

(send bot subscribe-message-event
      (λ (event)
        (define subject (send event get-subject))
        (define mcb (new message-chain-builder%))
        (define sent? #f)
        (define add-message (create-add-message
                             mcb
                             (λ (_) (cond
                                      [sent? (error) #f]
                                      [else #t]))))

        (define message (send event get-message))
        (when (non-empty-string? (send message content-to-string))
          (define handled (handle-eval-service-message event add-message))
          (when (not handled)
            (set! handled (handle-message bot event add-message)))
          (when handled
            (define mc (send mcb build))
            (when (not (send mc empty?))
              (send subject send-message (send mc trim))))
          (set! sent? #t))))

(displayln "连接中...")
(send bot login
      (λ ()
        (displayln "已连接到服务器:)")
        (handle-login bot (notify-mode))
        (dict-add-word bot-nickname)))