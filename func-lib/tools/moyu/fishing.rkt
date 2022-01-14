#lang racket
(require math/base
         2htdp/image
         (except-in racket/draw make-color make-pen)
         (only-in mrlib/image-core render-image)
         "fish-manager.rkt"
         "fish-basket.rkt"
         "config.rkt"
         "../../../chat/message/main.rkt"
         "../../../chat/contact/main.rkt"
         "../../utils/random.rkt")

(provide fishing-game%
         (struct-out s-fished-fish))


(struct s-fished-fish s-fish (weight))

(define fishing-game%
  (class object%
    (super-new)

    (define users (make-hash))
    (define user-fish-baskets (make-hash))

    (define/public (fishing user)
      (cond
        [(= (random-integer 0 2) 0)
         (define user-id (send user get-id))
         (define fish (list-random-ref (fish-mgr:get-fishes)))
         (define fish-weight (/ (random (inexact->exact (* (s-fish-weight-min fish) 1000))
                                        (inexact->exact (+ (* (s-fish-weight-max fish) 1000) 1)))
                                1000))
         (define fish-basket (fish-basket-mgr:get-fish-basket-by-user-id user-id))
         (when (not fish-basket)
           (set! fish-basket (fish-basket-mgr:create-fish-basket user-id 60)))
         (cond
           [(send fish-basket full?) 2]
           [else
            (send fish-basket add-fish (s-fish-id fish) fish-weight)
            (cond
              [(send fish-basket full?) 3]
              [else
               (s-fished-fish (s-fish-id fish)
                              (s-fish-name fish)
                              (s-fish-alias fish)
                              (s-fish-weight-min fish)
                              (s-fish-weight-max fish)
                              (s-fish-appear-pr fish)
                              (s-fish-price fish)
                              fish-weight)])])]
        [else #f]))
    
    (define/public (stat-user subject user add-message)
      (define user-id (send user get-id))
      (define fish-basket (fish-basket-mgr:get-fish-basket-by-user-id user-id))
      (when (is-a? subject group%)
        (add-message (new at% [target user-id]))
        (add-message " "))
      (cond
        [fish-basket
         (define (render-stat-image stat-result)
           (define stat-items (s-stat-result-items stat-result))
           (define item-width 120)
           (define item-height 120)
           (define item-col-num 4)
           (define item-row-num (ceiling (/ (length stat-items) item-col-num)))
           (define width (min (* item-col-num item-width)
                              (* (length stat-items) item-width)))
           (define height (max item-height (* item-row-num item-height)))
           (define bm (make-bitmap width height))
           (define big-dc (send bm make-dc))
           (define item-index 0)
           (for-each
            (λ (stat-item)
              (define fish (s-stat-item-fish stat-item))
              (define fish-bm (read-bitmap (get-fish-image-path-by-id (s-fish-id fish))))
              (define item-x (* (remainder item-index item-col-num) item-width))
              (define item-y (* (floor (/ item-index item-col-num)) item-height))
              (send big-dc draw-bitmap-section-smooth fish-bm
                    item-x item-y item-width item-height
                    0 0 (send fish-bm get-width) (send fish-bm get-height))

              (define text-image
                (above/align "left"
                             (text/font (s-fish-name fish)
                                        11 "black" "FZLanTingHeiS-R-GB" 'modern 'normal 'bold #f)
                             (beside/align "bottom"
                                           (text/font (format "共~a条" (s-stat-item-count stat-item))
                                                      11 "red" "FZLanTingHeiS-R-GB" 'modern 'normal 'normal #f)
                                           (text/font (format " ~a公斤" (~r (s-stat-item-weight stat-item) #:precision 2))
                                                      11 "crimson" "FZLanTingHeiS-R-GB" 'modern 'normal 'normal #f))))
              (define text-bm (make-bitmap (image-width text-image) (image-height text-image)))
              (define text-bdc (send text-bm make-dc))
              (render-image text-image text-bdc 0 0)
              (send big-dc draw-bitmap text-bm item-x item-y)
              (set! item-index (+ item-index 1)))
            stat-items)
           bm)
         (define stat-result (send fish-basket stat))
         (cond
           [(> (s-stat-result-count stat-result) 0)
            (add-message (format "摸到鱼共计~a条，~a公斤。\n"
                                 (s-stat-result-count stat-result)
                                 (~r (s-stat-result-weight stat-result) #:precision 2)))
            (define image (render-stat-image stat-result))
            (define path (build-path work-path "stat.png"))
            (send image save-file path 'png 100)
            (add-message (new image-message% [path path]))]
           [else
            (add-message "你没有鱼")])]
        [else (add-message "你未摸过鱼")]))

    (define/public (clear-fish-basket subject user add-message)
      (define user-id (send user get-id))
      (define fish-basket (fish-basket-mgr:get-fish-basket-by-user-id user-id))
      (when (is-a? subject group%)
        (add-message (new at% [target user-id]))
        (add-message " "))
      (cond
        [fish-basket
         (send fish-basket clear)
         (add-message "鱼已清空")]
        [else
         (add-message "你未摸过鱼")]))

    (define/public (ranking by top add-message)
      (add-message "摸鱼排名")
      (add-message (case by
                     [('count) "按条数"]
                     [('weight) "按重量"]
                     [('overall) ""]))
      (add-message (format " TOP ~A\n" top))
      (define no 1)
      (for-each
       (λ (row)
         (match-define (list user-id nickname weight count) row)
         (add-message (format "#~A ~A ~A条 ~A公斤\n"
                              (~a no
                                  #:align 'left #:width 2 #:pad-string " ")
                              (~a nickname
                                  #:align 'left #:width 12 #:pad-string " ")
                              count
                              (~r weight #:precision 2)))
         (set! no (+ no 1)))
       (query-ranking-by by top)))))

(module ranking racket
  (require "../../../db/bot-db.rkt"
           "../../../db/db-util.rkt")

  (provide query-ranking-by)
  
  (define (query-ranking-by by top)
    (connect-db)
    (define (mapping-result result)
      (map-rows-result
       result
       (λ (get-value)
         (list (get-value "user_id")
               (if (sql-null? (get-value "nickname"))
                   (format "@~a" (get-value "user_id"))
                   (get-value "nickname"))
               (get-value "weight")
               (get-value "count")))))
    (mapping-result
     (query db-conn (string-append "select b.user_id, (select nickname from user where id = b.user_id) nickname,"
                                   "sum(bf.fish_weight) weight, count(bf.fish_id) count "
                                   "from moyu_fish_basket_fish bf "
                                   "inner join moyu_fish_basket b on bf.basket_id = b.id "
                                   "group by b.user_id "
                                   "order by "
                                   (case by
                                     [(weight) "weight desc"]
                                     [(count) "count desc"]
                                     [(overall) "weight desc, count desc"])
                                   " limit $1")
            top))))
(require 'ranking)