#lang racket
(require "cut-word.rkt")

(provide cut-tagged-words
         (struct-out tagged-word))


(define (print/join-tagged-words words)
  (displayln (string-join
              (map (λ (w)
                     (if (equal? (tagged-word-type w) 'text)
                         (tagged-word-data w)
                         (format "[~a:~a]" (tagged-word-type w) (tagged-word-data w))))
                   words)
              "/" #:before-first "/" #:after-last "/")))

(struct tagged-word (type data) #:transparent)

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

  (define (box-copy-value! i j)
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
  (define (parse-time i)
    (define (parse-time/hour i)
      (define word (read-word i))
      (cond
        [(and word (equal? (s-word-type word) 'number))
         (define hour (string->number (s-word-text word)))
         (define ahead-i (add i 1))
         (define ahead (read-word ahead-i)) ;ahead-i为跳过空格后的位置
         (match (and ahead (s-word-text ahead))
           [(or ":" "\uff1a" "点")
            (box-copy-value! i ahead-i)
            (advance! i)
            (define minute (parse-time/minute i))
            (cond
              [minute
               (append (list (cons 'hour hour)) minute)]
              [else
               (define second (parse-time/second i))
               (append (list (cons 'hour hour)) (if second second null))])]
           [_ #f])]
        [else #f]))

    (define (parse-time/minute i)
      (define word (read-word i))
      (cond
        [(and word (equal? (s-word-type word) 'number))
         (define minute (string->number (s-word-text word)))
         (define ahead-i (add i 1))
         (define ahead (read-word ahead-i))
         (match (and ahead (s-word-text ahead))
           [(or ":" "\uff1a" "分" "分钟")
            (box-copy-value! i ahead-i)
            (define second (parse-time/second (advance! i)))
            (append (list (cons 'minute minute)) (if second second null))]
           [_ #f])]
        [else #f]))

    (define (parse-time/second i)
      (define word (read-word i))
      (cond
        [(and word (equal? (s-word-type word) 'number))
         (define second (string->number (s-word-text word)))
         (define ahead-i (add i 1))
         (define ahead (read-word ahead-i))
         (match (and ahead (s-word-text ahead))
           [(or ":" "\uff1a" "秒")
            (box-copy-value! i ahead-i)
            (list (cons 'second second))]
           [_ #f])]
        [else #f]))

    (define head (read-word i))
    (define ahead (read-word (add i 1)))
    (cond
      [(and head ahead (equal? (s-word-type head) 'number))
       (define ret
         (match (s-word-text ahead)
           [(or ":" "\uff1a" "点" "时") (parse-time/hour i)]
           ["分" (parse-time/minute i)]
           ["秒" (parse-time/second i)]
           [_ #f]))
       (if ret (make-hash ret) #f)]
      [else #f]))

  (let ([i (box 0)]
        [result null])
    (define (add-word type data)
      (when (or (not (equal? type 'space)) (not remove-space?))
        (set! result (append result (cons (tagged-word type data) null)))))
    (define (add-raw-word word)
      (add-word (s-word-type word) (s-word-text word)))
    
    (let loop ()
      (when (< (unbox i) word-count)
        (define word (read-word i #f))
        (match (s-word-type word)
          ['number
           (define time (parse-time i))
           (if time
               (add-word 'time time)
               (add-raw-word word))]
          [else
           (add-raw-word word)])
        (advance! i)
        (loop)))
    result))

;test
;(define sentence "在 3 ： 12    ：  56秒  xx")
;(print/join-words (cut-words sentence))
;(print/join-tagged-words (cut-tagged-words sentence))