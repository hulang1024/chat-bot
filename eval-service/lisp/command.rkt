#lang racket
(require "boot/main.rkt")

(provide handle-command)


(define (handle-command sender command)
  (case (hash-ref command 'name)
    [("save-boot-code")
     (define code (hash-ref (hash-ref command 'args) 'code))
     (save-boot-code sender code)
     "ok"]
    [else
     (format "未知命令: ~a" (hash-ref command 'name))]))