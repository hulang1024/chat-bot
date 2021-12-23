#lang racket
(require "../../contact/group.rkt"
         "../../contact/friend.rkt")

(provide decode-group
         decode-group-member
         decode-friend)


(define (decode-group-member jsexpr bot)
  (match-define (hash-table ('id id)
                            ('memberName member-name)
                            ('specialTitle special-title)
                            ('group group-jsexpr)) jsexpr)
  (new group-member%
       [id id]
       [bot bot]
       [member-name member-name]
       [special-title special-title]
       [group (decode-group group-jsexpr bot)]))


(define (decode-group jsexpr bot)
  (match-define  (hash-table ('id id)
                             ('name name)) jsexpr)
  (new group%
       [id id]
       [bot bot]
       [name name]))


(define (decode-friend jsexpr bot)
  (match-define (hash-table ('id id)
                            ('nickname nickname)
                            ('remark remark)) jsexpr)
  (new friend%
       [id id]
       [bot bot]
       [nickname nickname]
       [remark remark]))
    