#lang racket

(require "../../../db/bot-db.rkt"
         "../../../db/db-util.rkt")

(provide (prefix-out boot: query-all)
         (prefix-out boot: save)
         (struct-out s-boot-code))

; 提醒
(struct s-boot-code (; 用户id
                     user-id
                     ; 代码
                     code
                     ; 增加时间
                     add-at)
  #:transparent)


(define (query-all)
  (query-boot-codes "select * from boot_code order by add_at asc"))


(define (query-boot-codes stmt . args)
  (connect-db)
  (define (mapping-result result)
    (map-rows-result
     result
     (λ (get-value)
       (s-boot-code (get-value "user_id")
                    (get-value "code")
                    (get-value "add_at")))))
  (mapping-result (apply query db-conn stmt args)))

(define (save record)
  (connect-db)
  (query-exec db-conn
              "insert into boot_code(user_id, code, add_at) values($1, $2, datetime('now'))"
              (s-boot-code-user-id record)
              (s-boot-code-code record)))