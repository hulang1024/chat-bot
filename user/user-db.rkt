#lang racket
(require "../db/bot-db.rkt"
         "../db/db-util.rkt")

(provide (struct-out s-user)
         (prefix-out user-repo: add-or-update))


(struct s-user (id nickname update-at) #:transparent)

(define (add-or-update user)
  (connect-db)
  (if (exists? (s-user-id user))
      (query-exec db-conn
                  (string-append
                   "update user set nickname = $1, update_at = datetime('now') "
                   "where id = $2")
                  (s-user-nickname user)
                  (s-user-id user))
      (add user)))

(define (add user)
  (connect-db)
  (query-exec db-conn
              "insert into user(id, nickname, update_at) values ($1, $2, datetime('now'))"
              (s-user-id user)
              (s-user-nickname user)))
  
(define (exists? id)
  (connect-db)
  (define ret (query-maybe-value db-conn "select count(1) from user where id = $1" id))
  (if (= ret 0) #f #t))