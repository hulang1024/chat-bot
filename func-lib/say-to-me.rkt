#lang racket

(provide say-to-me)

(define say-to-me
"可以跟我说：
'3分钟后叫我喝水' '10秒后叫@张三喝水' '8点提醒我改bug' '取消提醒'
'上海天气' '上海明天天气'
'笑话'
'图' '风景图' '动漫图' '美女图' '妹子图'
'随机点数'
'美音 hello' '英音 hello'
（要叫我或者at我，我才知道哦）
执行racket代码直接发送'#rkt racket代码'")