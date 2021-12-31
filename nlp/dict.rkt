#lang racket
(require "data.rkt")

(provide init-dict
         dict-max-word-length
         dict-has?
         wide-punctuation?)


(define dict null)
(define wide-punctuation-dict null)
(define dict-max-word-length 2)

(define (init-dict)
  (set! dict (append (read-data-file "dict.txt")
                     (read-data-file "city-names.txt")))
  (set! dict-max-word-length
        (let ([max 0])
          (for-each
           (λ (word)
             (define len (string-length word))
             (when (> len max)
               (set! max len)))
           dict)
          max))
  (set! wide-punctuation-dict (map (λ (s) (string-ref s 0))
                                   (read-data-file "wide-punctuation.txt"))))

(init-dict)

(define (dict-has? word)
  (findf (λ (w) (string=? word w)) dict))

(define (wide-punctuation? ch)
  (findf (λ (c) (char=? ch c)) wide-punctuation-dict))
  