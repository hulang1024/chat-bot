#lang racket
(require net/rfc6455
         web-server/private/timer
         "command.rkt")

(provide start-heartbeat)


; 心跳间隔秒
(define intervel (- (ws-idle-timeout) 10))
(define timer-manager #f)
(define timer #f)

(define (start-heartbeat ws-conn)
  (when (not timer-manager)
    (set! timer-manager (start-timer-manager)))

  (set! timer (start-timer
               timer-manager
               intervel
               (λ () 
                 (timer-action ws-conn)
                 (start-heartbeat ws-conn)))))

(define (timer-action conn)
  (client-send-command! conn "about" #:has-sync-id #f))
  