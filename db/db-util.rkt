#lang racket
(require db/base)

(provide map-rows-result)


(define (map-rows-result result map-row)
  (define headers (rows-result-headers result))
  (define rows (rows-result-rows result))
  (define ((make-row-reader row) name)
    (define index
      (index-where headers
                   (λ (h) (string=? name (cdr (assoc 'name h))))))
    (vector-ref row index))
  (map (λ (row)
         (map-row (make-row-reader row)))
       rows))