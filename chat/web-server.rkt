#lang racket

(require web-server/servlet
         web-server/servlet-env
         json
         "./message/main.rkt"
         "./contact/group.rkt")

(provide init-web-server)

(define _bot null)
 
(define (main-server req)
  (define req-data (bytes->jsexpr (request-post-data/raw req)))
  (define text (hash-ref req-data 'text))
  (define group-id (hash-ref req-data 'group_id))

  (define subject (new group% [bot _bot] [id group-id]))
  (define mcb (new message-chain-builder%))
  (define add-message (create-add-message mcb))
  (add-message (new mirai-code-message% [code text]))
  (send subject send-message (send mcb build))

  (response 200 #"OK"
            (current-seconds)
            APPLICATION/JSON-MIME-TYPE
            empty
            (λ (out) (write-string "ok" out))))

(define (init-web-server bot)
  (set! _bot bot)
  (define server-ip "127.0.0.1")
  (define server-port 8084)
  (thread (λ ()
    (serve/servlet main-server
                   #:port server-port
                   #:listen-ip server-ip
                   #:command-line? #t
                   #:servlet-path "/"))))