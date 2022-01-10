#lang racket
(require db
         "config.rkt"
         "../../../db/db-util.rkt")

(provide (struct-out s-remind)
         (prefix-out remind-mgr: query-all)
         (prefix-out remind-mgr: query-by-id)
         (prefix-out remind-mgr: query-by-create-uid)
         (prefix-out remind-mgr: query-by-target-uid)
         (prefix-out remind-mgr: generate-id)
         (prefix-out remind-mgr: save)
         (prefix-out remind-mgr: delete)
         (prefix-out remind-mgr: delete-by-create-uid))


; 提醒
(struct s-remind (id
                  ; 提醒时间
                  time
                  ; 提醒用户id，为0表示无特定用户
                  target-uid
                  ; 提醒消息发送的群组，如果为0：同时target-uid不为0则表示私发，否则表示全部群
                  target-group-id
                  ; 响铃重复次数
                  repeat-times
                  ; 内容
                  content
                  ; 创建用户id
                  create-uid
                  ; 创建时间
                  create-at)
  #:transparent)


(define db-conn #f)

(define (connect-db)
  (when (not db-conn)
    (set! db-conn (sqlite3-connect #:database db-path))))

(define (query-all)
  (query-reminds "select * from remind order by create_at asc"))

(define (query-by-id id)
  (define l (query-reminds "select * from remind where id = $1" id))
  (if (null? l) #f (first l)))

(define (query-by-create-uid uid)
  (query-reminds "select * from remind where create_uid = $1 order by create_at asc" uid))

(define (query-by-target-uid target-uid)
  (query-reminds "select * from remind where target_uid = $1 order by create_at asc" target-uid))

(define (query-reminds stmt . args)
  (connect-db)
  (define (mapping-result result)
    (map-rows-result
     result
     (λ (get-value)
       (s-remind (get-value "id")
                 (get-value "remind_time")
                 (get-value "target_uid")
                 (get-value "target_group_id")
                 (get-value "repeat_times")
                 (get-value "content")
                 (get-value "create_uid")
                 (get-value "create_at")))))
  (mapping-result (apply query db-conn stmt args)))


(define generate-id (compose floor current-inexact-milliseconds))

(define (save remind)
  (connect-db)
  (query-exec db-conn
              (string-append "insert into remind"
                             "(id, remind_time, target_uid, target_group_id, repeat_times, content, create_uid, create_at)"
                             "values ($1, $2, $3, $4, $5, $6, $7, datetime('now'))")
              (s-remind-id remind)
              (s-remind-time remind)
              (s-remind-target-uid remind)
              (s-remind-target-group-id remind)
              (s-remind-repeat-times remind)
              (s-remind-content remind)
              (s-remind-create-uid remind)))


(define (delete id)
  (connect-db)
  (query-exec db-conn "delete from remind where id = $1" id))


(define (delete-by-create-uid uid)
  (connect-db)
  (query-exec db-conn "delete from remind where create_uid = $1" uid))