#lang racket
(require "dict.rkt") 

(provide (rename-out (cut-words/mm cut-words))
         print/join)
         

(define (print/join words)
  (displayln (string-join words "/" #:before-first "/" #:after-last "/")))


; 基于词典的切分，双向最大匹配算法
(define (cut-words/mm sentence)
  (set! sentence (string-trim sentence))
  (define len (string-length sentence))

  ; reverse? #t为反向，#f为正向
  (define (cut reverse?)
    (define (read-section out pred i)
      (define cs null)
      (let loop ()
        (define c (string-ref sentence i))
        (when (pred c)
          (cond
            [reverse?
             (set! cs (cons c cs))
             (set! i (- i 1))
             (when (>= i 0)
               (loop))]
            [else
             (set! cs (append cs (cons c null)))
             (set! i (+ i 1))
             (when (< i len)
               (loop))])))
      (set-box! out (list->string cs))
      i)
  
    (define (read-number out i)
      (read-section out char-numeric? i))
  
    (define (read-spaces out i)
      (read-section out char-whitespace? i))

    (define (read-alpha-word out i)
      (read-section out alpha-word-char? i))

    (define (alpha-word-char? c)
      (or (char-upper-case? c) (char-lower-case? c)))

    (define words null)
    (define add-word
      (if reverse?
          (λ (word)
            (set! words (cons word words)))
          (λ (word)
            (set! words (append words (cons word null))))))
    (define start (if reverse? (- len 1) 0))
    (define word-len 0)
    (define sub null)

    (let loop ()
      (let loop-non-han ()
        (define c (string-ref sentence start))
        (define is-han #f)
        (define out (box #f))
        (match c
          [(? char-numeric? c)
           (set! start (read-number out start))]
          [(? char-whitespace? c)
           (set! start (read-spaces out start))]
          [(? alpha-word-char? c)
           (set! start (read-alpha-word out start))]
          [_ (set! is-han #t)])
        (when (unbox out)
          (add-word (unbox out)))
        (when (and (if reverse? (>= start 0) (< start len))
                   (not is-han))
          (loop-non-han)))

      (when (if reverse? (>= start 0) (< start len))
        (set! word-len (min dict-max-word-length (if reverse? (+ start 1) (- len start))))
        (let loop ()
          (set! sub (if reverse?
                        (substring sentence (+ 1 (- start word-len)) (+ start 1))
                        (substring sentence start (+ start word-len))))
          (cond
            [(= word-len 1) (add-word sub)]
            [(dict-has? sub) (add-word sub)]
            [else
             (set! word-len (- word-len 1))
             (when (> word-len 0)
               (loop))]))
        (set! start ((if reverse? - +) start word-len)))
      (when (if reverse? (>= start 0) (< start len))
        (loop)))
    words)
  
  (define (single-count words)
    (count (λ (word) (= (string-length word) 1))
           words))
  
  (define f-words (cut #f))
  (define b-words (cut #t))
  (define f-total (length f-words))
  (define b-total (length b-words))
  ; 对正向和反向的结果进行比较，使用启发式规则决定采用哪一种结果
  (cond
    ; 如果词数相同
    [(= f-total b-total)
     (cond
       ; 如果分词内容相同，说明没有歧义，返回任意一个
       [(andmap string=? f-words b-words) f-words]
       [else
        (define f-cnt (single-count f-words))
        (define b-cnt (single-count b-words))
        ; 否则，返回单字词较少的那个
        (if (>= f-cnt b-cnt)
            b-words
            f-words)])]
    [else
     ; 否则，返回词数较少的那个
     (if (> f-total b-total)
         b-words
         f-words)]))