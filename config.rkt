#lang racket

(provide bot-id
         bot-nickname
         admin-ids
         mirai-ws-server-config)


(define bot-id 3381775672)
(define bot-nickname "喵喵")
(define admin-ids '(1013644379))

(define mirai-ws-server-config
  (hash 'hostname "localhost"
        'port 8899))