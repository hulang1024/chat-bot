#lang racket
(require db
         "config.rkt"
         "../../../db/db-util.rkt")

(provide s-remind
         (prefix-out remind-repo: query-all)
         (prefix-out remind-repo: query-by-create-uid)
         (prefix-out remind-repo: save)
         (prefix-out remind-repo: delete)
         (prefix-out remind-repo: delete-by-create-uid))


(struct s-remind (id
                  time
                  target-uid
                  target-group-id
                  repeat-times
                  content
                  create-uid
                  create-at)
  #:transparent)


(define db-conn #f)

(define (connect-db)
  (when (not db-conn)
    (set! db-conn (sqlite3-connect #:database db-path))))

(define (query-all)
  (query-reminds "select * from remind"))

(define (query-by-create-uid uid)
  (query-reminds "select * from remind where create_uid = $1" uid))

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


(define (save remind)
  (connect-db)
  (query-exec db-conn
              (string-append "insert into remind"
                             "(remind_time, target_uid, target_group_id, repeat-times, content, create_uid, create_at)"
                             "values ($1, $2, $3, $4, $5, datetime('now'))")
              (s-remind-time remind)
              (s-remind-target-uid remind)
              (s-remind-target-group-id remind)
              (s-remind-repeat-times remind)
              (s-remind-content remind)
              (s-remind-create-uid remind)
              (s-remind-create-at remind)))


(define (delete id)
  (connect-db)
  (query-exec db-conn "delete from remind where id = $1" id))


(define (delete-by-create-uid uid)
  (connect-db)
  (query-exec db-conn "delete from remind where create_uid = $1" uid))