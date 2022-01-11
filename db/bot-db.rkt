#lang racket
(require db
         "config.rkt")

(provide connect-db
         db-conn
         (all-from-out db))


(define db-conn #f)

(define (connect-db)
  (when (not db-conn)
    (set! db-conn (sqlite3-connect #:database db-path))))