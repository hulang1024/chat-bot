#lang racket
(require net/head)

(provide get-header-value)


(define (get-header-value name headers)
  (define header (findf (λ (h) (extract-field name h)) headers))
  (bytes->string/utf-8 (extract-field name header)))