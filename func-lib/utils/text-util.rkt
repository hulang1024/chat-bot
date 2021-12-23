#lang racket

(provide text-width)


(define (text-width text)
  (define (ch-width ch)
    (if (> (char->integer ch) 127) 2 1))
  (apply + (map (λ (ch) (ch-width ch)) (string->list text))))
