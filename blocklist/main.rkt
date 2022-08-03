#lang racket

(require "../db/bot-db.rkt"
         "../db/db-util.rkt")

(provide (prefix-out blocklist: exists?)
         (prefix-out blocklist: reload-all))

; 提醒
(struct s-block-user (; 用户id
                      id
                      ; 类型
                      type
                      ; 增加时间
                      add-at)
  #:transparent)


(define block-users #f)

(define (cache-all)
  (when (not block-users)
    (reload-all)))

(define (reload-all)
  (set! block-users (query-all)))

(define (query-all)
  (query-block-users "select * from block_user order by add_at desc"))

(define (exists? user-id type)
  (cache-all)
  (findf (λ (t)
           (and (eq? (s-block-user-id t) user-id)
                (eq? (s-block-user-type t) type)))
         block-users))


(define (query-block-users stmt . args)
  (connect-db)
  (define (mapping-result result)
    (map-rows-result
     result
     (λ (get-value)
       (s-block-user (get-value "id")
                     (get-value "type")
                     (get-value "add_at")))))
  (mapping-result (apply query db-conn stmt args)))

(define (save record)
  (connect-db)
  (query-exec db-conn
              "insert into block_user(id, type, add_at) values($1, $2, datetime('now')"
              (s-block-user-id record)
              (s-block-user-type record)))