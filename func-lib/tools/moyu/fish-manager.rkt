#lang racket
(require "config.rkt"
         "../../../db/bot-db.rkt"
         "../../../db/db-util.rkt")

(provide (struct-out s-fish)
         (prefix-out fish-mgr: reload-all)
         (prefix-out fish-mgr: get-fishes)
         get-fish-image-path-by-id)


(struct s-fish (id name alias weight-min weight-max appear-pr price) #:transparent)
; 所有鱼
(define fishes #f)

; 根据id获取图片路径
(define (get-fish-image-path-by-id id)
  (build-path work-path "fish-images" (format "~a.png" id)))


(define (get-fishes)
  (when (not fishes)
    (reload-all))
  fishes)


(define (reload-all)
  (set! fishes (query-fishs "select * from fish")))


(define (save fish)
  (connect-db)
  (query-exec db-conn
              (string-append "insert into fish(id, name) values ($1, $2)")
              (s-fish-id fish)
              (s-fish-name fish)))


(define (query-fishs stmt . args)
  (connect-db)
  (define (mapping-result result)
    (map-rows-result
     result
     (λ (get-value)
       (s-fish (get-value "id")
               (get-value "name")
               (get-value "alias")
               (get-value "weight_min")
               (get-value "weight_max")
               (get-value "appear_pr")
               (get-value "price")))))
  (mapping-result (apply query db-conn stmt args)))
