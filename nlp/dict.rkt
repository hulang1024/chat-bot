#lang racket
(require "../data/citys.rkt")

(provide dict
         dict-max-word-length
         dict-has?)


(define dict
  '(前天
    昨天
    今天
    明天
    后天
    分钟
    小时
    一刻
    时候
    天气
    提醒
    笑话
    图片
    风景
    动漫
    动画
    妹子
    设置
    取消
    菜单
    美音
    英音
    随机
    点数
    用户
    osu
    osu-stat))

(set! dict (append dict city-names))
(define dict-max-word-length 2)
(for-each
 (λ (word)
   (define len (string-length (symbol->string word)))
   (when (> len dict-max-word-length)
     (set! dict-max-word-length len)))
 dict)


(define (dict-has? word)
  (set! word (string->symbol word))
  (findf (λ (w) (equal? word w)) dict))