#lang racket
(require math/base
         "fish-manager.rkt"
         "fish-basket.rkt"
         "../../utils/random.rkt")

(provide fishing-game%
         (struct-out s-fished-fish))


(struct s-fished-fish s-fish (weight))

(define fishing-game%
  (class object%
    (super-new)

    (define users (make-hash))
    (define user-fish-baskets (make-hash))

    (define/public (fishing user add-message)
      (define user-id (send user get-id))
      (define fish (list-random-ref (fish-mgr:get-fishes)))
      (define fish-weight (max (floor (* (random) 100)) 1))
      (add-message (format "摸到了一条重~a的~a\n"
                           (~r (s-fished-fish-weight fish) #:precision 2)
                           (s-fish-name fish)))
      (define fish-basket (fish-basket-mgr:get-fish-basket-by-user-id user-id))
      (when (not fish-basket)
        (set! fish-basket (fish-basket-mgr:create-fish-basket user-id 60)))
      (cond
        [(send fish-basket full?)
         (add-message "放入鱼护失败，原因：鱼护满了")]
        [else
         (send fish-basket add-fish (s-fish-id fish) fish-weight)
         (when (send fish-basket full?)
           (add-message "你的鱼护满了"))])
      (s-fished-fish fish fish-weight))
    
    (define/public (stat user add-message)
      (define user-id (send user get-id))
      (define fish-basket (fish-basket-mgr:get-fish-basket-by-user-id user-id))
      (cond
        [fish-basket
         (add-message (format "~A 的鱼的统计" (send user get-nickname)))
         (render-stat-image (send fish-basket stat))]
        [else
         (add-message "你未摸过鱼")]))

    (define/public (clear-fish-basket user add-message)
      (define user-id (send user get-id))
      (define fish-basket (fish-basket-mgr:get-fish-basket-by-user-id user-id))
      (cond
        [fish-basket
         (send fish-basket clear)
         (add-message (format "~A的鱼已清空" (send user get-nickname)))]
        [else
         (add-message "你未摸过鱼")]))

    (define/public (ranking by top display)
      (void))

    (define (render-stat-image stat-result)
      (void))))