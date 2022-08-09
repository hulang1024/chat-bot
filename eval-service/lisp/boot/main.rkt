#lang racket
(require "db.rkt")

(provide save-boot-code)


(define (save-boot-code sender code)
  (boot:save (s-boot-code (send sender get-id)
                     code
                     #f)))