#lang racket
(require "../../../db/bot-db.rkt"
         "../../../db/db-util.rkt"
         "fish-manager.rkt")

(provide (prefix-out fish-basket-mgr: create-fish-basket)
         (prefix-out fish-basket-mgr: get-fish-basket-by-user-id)
         s-stat-item
         s-stat-result)

(define (create-fish-basket user-id capacity)
  (connect-db)
  (define id user-id)
  (define fish-basket (new fish-basket%
                           [id id]
                           [user-id user-id]
                           [capacity capacity]))
  (query-exec db-conn
              (string-append
               "insert into moyu_fish_basket(id, user_id, capacity, create_at)"
               "values($1, $2, $3, datetime('now'))")
              id user-id capacity))

(define (get-fish-basket-by-user-id user-id)
  (connect-db)
  (define row (query-maybe-row db-conn
                               "select id, user_id, capacity, create_at from moyu_fish_basket where user_id = $1"
                               user-id))
  (cond
    [row
     (match-define (vector id _ capacity create_at) row)
     (new fish-basket% [id id] [user-id user-id] [capacity capacity])]
    [else #f]))
  

(struct s-stat-item (fish weight count) #:transparent)
(struct s-stat-result (items weight count) #:transparent)

(define fish-basket%
  (class object%
    (super-new)
    (init-field id user-id capacity)
    
    (define/public (get-id) id)
    (define/public (get-user-id) user-id)

    (define/public (add-fish fish-id weight)
      (when (not (full?))
        (fish-basket-fish-repo:save (s-fish-basket-fish id fish-id weight null))))

    (define/public (full?)
      (define count (fish-basket-fish-repo:count-by-basket-id id))
      (= count capacity))

    (define/public (clear)
      (fish-basket-fish-repo:delete-by-basket-id id))

    (define/public (stat)
      (define stat-items null)
      (define count 0)
      (define weight 0)
      (for-each
       (λ (item)
         (match-define (list id name alias kind-weight-sum kind-count) item)
         (define fish (s-fish id name alias))
         (define stat-item (s-stat-item fish kind-weight-sum kind-count))
         (set! stat-items (append stat-items (cons stat-item null)))
         (set! weight (+ weight kind-weight-sum))
         (set! count (+ count kind-count)))
       (fish-basket-fish-repo:stat-by-basket-id id))
      (s-stat-result stat-items weight count))))


(module fish-basket-fish-repo racket
  (require "../../../db/bot-db.rkt"
           "../../../db/db-util.rkt")
  
  (provide s-fish-basket-fish
           (prefix-out fish-basket-fish-repo: save)
           (prefix-out fish-basket-fish-repo: count-by-basket-id)
           (prefix-out fish-basket-fish-repo: delete-by-basket-id)
           (prefix-out fish-basket-fish-repo: stat-by-basket-id))
  
  (struct s-fish-basket-fish (basket-id fish-id fish-weight add-at))

  (define (save record)
    (connect-db)
    (query-exec db-conn
                (string-append
                 "insert into moyu_fish_basket_fish(basket_id, fish_id, fish_weight, add_at)"
                 "values($1, $2, $3, datetime('now'))")
                (s-fish-basket-fish-basket-id record)
                (s-fish-basket-fish-fish-id record)
                (~r (s-fish-basket-fish-fish-weight record) #:precision 2)))

  (define (count-by-basket-id basket-id)
    (connect-db)
    (query-value db-conn "select count(1) from moyu_fish_basket_fish where basket_id = $1" basket-id))

  (define (delete-by-basket-id basket-id)
    (connect-db)
    (query-exec db-conn "delete from moyu_fish_basket_fish where basket_id = $1" basket-id))

  (define (stat-by-basket-id basket-id)
    (connect-db)
    (define (mapping-result result)
      (map-rows-result
       result
       (λ (get-value)
         (list (get-value "id")
               (get-value "name")
               (get-value "alias")
               (get-value "weight")
               (get-value "count")))))
    (mapping-result
     (query db-conn (string-append "select fish.*, sum(fish_weight) weight, count(fish_id) count "
                                   "from moyu_fish_basket_fish "
                                   "inner join fish on fish_id = fish.id "
                                   "where basket_id = $1 "
                                   "group by fish_id")
            basket-id))))

(require 'fish-basket-fish-repo)