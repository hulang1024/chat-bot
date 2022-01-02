#lang racket
(require "cut-word.rkt")

(provide cut-tagged-words
         (struct-out tagged-word)
         (struct-out tagged-word/text)
         (struct-out time-expr)
         (struct-out hour-section))


(define (print/join-tagged-words words)
  (displayln (string-join
              (map (λ (w)
                     (if (equal? (tagged-word-type w) 'text)
                         (tagged-word-data w)
                         (format "[~a:~a]" (tagged-word-type w) (tagged-word-data w))))
                   words)
              "/" #:before-first "/" #:after-last "/")))

(struct tagged-word (type data) #:transparent)

(struct tagged-word/text tagged-word (text))

(struct time-expr
  (; 表达运算
   op
   ; 时间分量名
   part-name
   ; 运算值
   value
   ; #t:是长度，#f:是时间点，null:歧义
   duration?) #:transparent)

(struct hour-section (start end) #:transparent)

;;; 分词，并标注词性
; remove-sapce? 是否移除 无意义 的空白
(define (cut-tagged-words sentence #:remove-space? [remove-space? #t])
  (define raw-words (cut-words sentence))
  (define word-count (length raw-words))

  ; 读一个单词
  ; skip-space? 是否跳过前面的空格，默认#t，如果为#t，则可能会修改 i 的值
  (define (read-word i [skip-space? #t])
    (when skip-space?
      (skip-space! i))
    (if (< (unbox i) word-count)
        (list-ref raw-words (unbox i))
        #f))

  ; 将索引 i 修改为 i+step
  (define (advance! i [step 1])
    (set-box! i (+ (unbox i) step))
    i)

  ; i+j，返回一个 新的 box
  (define (add i j)
    (box (+ (unbox i) j)))

  (define (copy-box! i j)
    (set-box! i (unbox j))
    i)
  
  ; 跳过空格，修改i为首个非空格的位置
  (define (skip-space! i)
    (when (< (unbox i) word-count)
      (let loop ([j (unbox i)])
        (define word (list-ref raw-words j))
        (cond
          [(equal? (s-word-type word) 'space)
           (if (< (+ j 1) word-count) (loop (+ j 1)) j)]
          [else (set-box! i j)]))))

  ;; 解析过程的设计
  ; (parse-<object-name> i ...)
  ; 参数 i 是当前索引，为了代码简单，索引存储在可变的box中，注意实现：可能会意外修改了其值。
  ; 如果解析失败，i的值将不会被修改，返回值为#f。
  ; 如果解析成功，i的值一般是解析对象的文本的最后一个字符终止位置(不会多1)，返回值为解析出的对象。
  ; 尽可能不要复制额外的box--因为这样就回到了问题起点，而且还因为box/unbox增加了额外的复杂性。

  ; 解析时间
  (define (parse-clock i)
    (define (hour-postfix? s)
      (and s (findf (λ (s1) (string=? s s1)) '(":" "\uff1a" "点" "点钟" "时"))))
    (define (minute-postfix? s)
      (and s (findf (λ (s1) (string=? s s1)) '(":" "\uff1a" "分" "分钟"))))
    (define (second-postfix? s)
      (and s (findf (λ (s1) (string=? s s1)) '(":" "\uff1a" "秒" "秒钟"))))
    
    (define (parse-clock/hour i duration?)
      (define word (read-word i))
      (cond
        [(and word (equal? (s-word-type word) 'number))
         (define hour (string->number (s-word-text word)))
         (define ahead-i (add i 1))
         (define ahead (read-word ahead-i)) ;ahead-i为跳过空格后的位置
         (match (and ahead (s-word-text ahead))
           [(or (? hour-postfix? _) "小时")
            (copy-box! i ahead-i)
            (define hour-expr (time-expr '= 'hour hour duration?))
            (set! ahead (read-word (advance! ahead-i)))
            (if ahead
                (match ahead
                  [(s-word 'text "半")
                   (copy-box! i ahead-i)
                   (list hour-expr (time-expr '= 'minute 30 duration?))]
                  [(s-word 'text "一刻")
                   (copy-box! i ahead-i)
                   (list hour-expr (time-expr '= 'minute 15 duration?))]
                  [else
                   (when (and ahead (string=? (s-word-text ahead) "过"))
                     (advance! ahead-i))
                   (define minute-expr (parse-clock/minute ahead-i #f duration?))
                   (cond
                     [minute-expr
                      (copy-box! i ahead-i)
                      (append (list hour-expr) minute-expr)]
                     [else
                      (define second-expr (parse-clock/second ahead-i #f duration?))
                      (cond
                        [second-expr
                         (copy-box! i ahead-i)
                         (list hour-expr second-expr)]
                        [else
                         hour-expr])])])
                hour-expr)]
           [else #f])]
        [else #f]))

    (define (parse-clock/minute i postfix? duration?)
      (define ahead-i (add i 0))
      (define word (read-word ahead-i))
      (cond
        [(and word (equal? (s-word-type word) 'number))
         (define minute (string->number (s-word-text word)))
         (define minute-expr (time-expr '= 'minute minute duration?))
         (define ahead (read-word (advance! ahead-i)))
         (match (and ahead (s-word-text ahead))
           [(? minute-postfix? _)
            (define j (add ahead-i 1))
            (define second-expr (parse-clock/second j #f duration?))
            (cond
              [second-expr
               (copy-box! i j)
               (list minute-expr second-expr)]
              [else
               (copy-box! i ahead-i)
               (list minute-expr)])]
           [else (if (or postfix? (and ahead (second-postfix? (s-word-text ahead))))
                     #f
                     (list minute-expr))])]
        [else #f]))

    (define (parse-clock/second i postfix? duration?)
      (define ahead-i (add i 0))
      (define word (read-word ahead-i))
      (cond
        [(and word (equal? (s-word-type word) 'number))
         (define second (string->number (s-word-text word)))
         (define second-expr (time-expr '= 'second second duration?))
         (define ahead (read-word (advance! ahead-i)))
         (match (and ahead (s-word-text ahead))
           [(? second-postfix? _)
            (copy-box! i ahead-i)
            second-expr]
           [else (if postfix? #f second-expr)])]
        [else #f]))

    (define head (read-word i))
    (define ahead (read-word (add i 1)))
    (cond
      [(and head ahead (equal? (s-word-type head) 'number))
       (match (s-word-text ahead)
         [(? hour-postfix? _) (parse-clock/hour i #f)]
         ["小时" (parse-clock/hour i #t)]
         [(? minute-postfix? _) (parse-clock/minute i #t null)]
         [(? second-postfix? _) (parse-clock/second i #t null)]
         [else #f])]
      [else #f]))

  (define (parse-relative-date i)
    (define ahead-i (add i 0))
    (define word (read-word ahead-i))
    (define ret
      (match (and word (s-word-text word))
        [(or "明天" "明日") (time-expr '+ 'day 1 #f)]
        [(or "后天" "后日") (time-expr '+ 'day 2 #f)]
        [(or "今天" "今日") (time-expr '+ 'day 0 #f)]
        [(or "昨天" "昨日") (time-expr '- 'day 1 #f)]
        [(or "前天" "前日") (time-expr '- 'day 2 #f)]
        ["明年" (time-expr '+ 'year 1 #f)]
        ["后年" (time-expr '+ 'year 2 #f)]
        ["今年" (time-expr '+ 'year 0 #f)]
        ["昨年" (time-expr '- 'year 1 #f)]
        ["前年" (time-expr '- 'year 2 #f)]
        [else #f]))
    (cond
      [ret (copy-box! i ahead-i)
           ret]
      [else #f]))

  (define (parse-hour-section i)
    (define ahead-i (add i 0))
    (define word (read-word ahead-i))
    (define ret
      (match (and word (s-word-text word))
        ["凌晨" (hour-section 0 2)]
        ["黎明" (hour-section 4 5)]
        ["拂晓" (hour-section 4 6)]
        ["清晨" (hour-section 6 7)]
        ["早晨" (hour-section 6 8)]
        ["早上" (hour-section 6 8)]
        ["上午" (hour-section 8 11)]
        ["中午" (hour-section 11 13)]
        ["下午" (hour-section 14 17)]
        ["晚上" (hour-section 18 22)]
        ["傍晚" (hour-section 17 18)]
        ["黄昏" (hour-section 16 17)]
        ["午夜" (hour-section 23 1)]
        ["夜间" (hour-section 19 5)]
        [else #f]))
    (cond
      [ret (copy-box! i ahead-i)
           ret]
      [else #f]))

  ; 返回一个连续时间词的列表
  (define (parse-time i)
    (define all-time-parsers (list parse-clock parse-relative-date parse-hour-section))
    (define time-exprs null)
    (define j (add i 0))
    (let loop ([time-parsers all-time-parsers])
      (define word (read-word j #f))
      (define ret
        (match (and word (s-word-text word))
          [(or "后" "之后") 'after]
          [(or "前" "之前") 'before]
          ["的" '-]
          [else ((car time-parsers) j)]))
      (cond
        [ret (set! time-exprs (append time-exprs
                                      (if (pair? ret) ret (cons ret null))))
             (copy-box! i j)
             (advance! j)
             (loop all-time-parsers)]
        [(not (null? (cdr time-parsers)))
         (loop (cdr time-parsers))]
        [else (advance! j -1) #f]))
    (cond
      [(null? time-exprs) #f]
      [else
       (copy-box! i j)
       time-exprs]))
  
  (let ([i (box 0)]
        [result null])
    (define (add-word type data text)
      (when (or (not (equal? type 'space)) (not remove-space?))
        (set! result (append result (cons (tagged-word/text type data text) null)))))
    (define (add-raw-word word)
      (add-word (s-word-type word)
                (if (equal? (s-word-type word) 'number)
                    (string->number (s-word-text word))
                    (s-word-text word))
                (s-word-text word)))

    (let loop ()
      (when (< (unbox i) word-count)
        (define start (unbox i))
        (define time (parse-time i))
        (cond
          [time
           (define text
             (string-join (map s-word-text
                               (take (list-tail raw-words start)
                                     (+ (- (unbox i) start) 1)))
                          ""))
           (add-word 'time time text)]
          [else
           (add-raw-word (read-word i #f))])
        (advance! i)
        (loop)))
    result))