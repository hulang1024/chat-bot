#lang racket

(require "../../contact/group.rkt"
         "../../contact/friend.rkt")

(provide decode-group
         decode-group-member
         decode-firend)


(define (decode-group-member jsexpr bot)
  (match-let jsexpr
    ([hash-table ('id id)
                 ('memberName member-name)
                 ('specialTitle special-title)
                 ('group group-jsexpr)])
    (new group-member%
         [id id]
         [bot bot]
         [member-name member-name]
         [special-title special-title]
         [group (decode-group group-jsexpr bot)])))

(define (decode-group jsexpr bot)
  (match-let jsexpr
    ([hash-table ('id id)
                 ('name name)])
    (new group%
         [id id]
         [bot bot]
         [name name])))

(define (decode-firend jsexpr bot)
  (match-let jsexpr
    ([hash-table ('id id)
                 ('nickname nickname)
                 ('remark remark)])
    (new friend%
         [id id]
         [bot bot]
         [nickname nickname]
         [remark remark])))
    