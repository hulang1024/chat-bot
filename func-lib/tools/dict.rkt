#lang racket
(require "../../chat/message/message.rkt")

(provide dict-a-voice
         dict-b-voice)


(define (dict-a-voice text add-message)
  (dict-voice 0 text add-message))

(define (dict-b-voice text add-message)
  (dict-voice 1 text add-message))

(define (dict-voice type text add-message)
  (define url (format "http://dict.youdao.com/dictvoice?type=~A&audio=~A" type text))
  (add-message (new voice-message% [url url])))
