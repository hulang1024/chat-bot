#lang racket
(require net/rfc6455
         json
         "command-response.rkt")

(provide client-send-command!)


(define command-sync-ids (make-hash))

(define (make-sync-id command)
  (define sync-id (+ (hash-ref! command-sync-ids command 0) 1))
  (hash-set! command-sync-ids command sync-id)
  sync-id)

(define (client-send-command! conn
                              command
                              #:content [content (json-null)]
                              #:sub-command [sub-command (json-null)]
                              #:has-sync-id [has-sync-id #t]
                              #:log? [log? #f])
  (define packet (make-hash `((command . ,command)
                              (subCommand . ,sub-command)
                              (content . ,content))))
  (define syncId (if has-sync-id
                     (format "~a-~a" command (make-sync-id command))
                     command))
  (hash-set! packet 'syncId syncId)
  (define json (jsexpr->string packet))
  (ws-send! conn json)
  (when log?
    (printf "-> ~a\n" json))
  (make-command-response-promise syncId))


