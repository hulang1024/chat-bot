#lang racket
(require "../../chat/message/message.rkt")

(provide get-random-pic)


(define (get-random-pic add-message)
  (add-message (new image-message% [url (get-random-pic-api-url)])))

(define (get-random-pic-api-url)
  "https://api.nmb.show/1985acg.php")