#lang racket

(provide admin-ids mirai-ws-server-config)

(define admin-ids '(1013644379))

(define mirai-ws-server-config
  (hash 'hostname "localhost"
        'port 8899))