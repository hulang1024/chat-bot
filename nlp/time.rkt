#lang racket

(define in-day-word-to-hour-range
  (hash
   '凌晨 '(0 . 2)
   '黎明 '(4 . 5)
   '拂晓 '(4 . 6)
   '清晨 '(6 . 7)
   '早晨 '(6 . 8)
   '上午 '(8 . 11)
   '中午 '(11 . 13)
   '下午 '(14 . 17)
   '晚上 '(18 . 22)
   '傍晚 '(17 . 18)
   '黄昏 '(16 . 17)
   '午夜 '(23 . 1)
   '夜间 '(19 . 5)))

(define relative-today-word-to-offset
  (hash
   '前天 -2
   '昨天 -1
   '今天 0
   '明天 1
   '后天 2))

(define duration-word-to-value #f)
   