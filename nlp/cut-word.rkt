#lang racket
(require "dict.rkt") 

(provide (rename-out (cut-words/mm cut-words))
         (struct-out s-word)
         print/join-words)
         

(define (print/join-words words)
  (displayln (string-join (map (λ (w)
                                 (format "[~a:~a]" (s-word-type w) (s-word-text w)))
                               words)
                          "/" #:before-first "/" #:after-last "/")))

(struct s-word (type text) #:transparent)

; 基于词典的的双向最大匹配分词算法
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
  
    (define (read-spaces out i)
      (read-section out char-whitespace? i))
    
    (define (read-en-word out i)
      ; todo: 待完善
      (read-section out en-word-char? i))

    (define (en-word-char? c)
      ; todo: 待完善
      (or (char-upper-case? c) (char-lower-case? c) (char=? #\- c)))


    (define (read-number out i)
      (define cs null)
      (define has-dot #f)
      (let loop ()
        (define c (string-ref sentence i))
        (define can-next #f)
        (cond
          [(char-numeric? c)
           (set! can-next #t)]
          [(char=? c #\-)
           (cond
             [reverse?
              (set! cs (cons c cs))
              (set! i (- i 1))
              #f]
             [(null? cs) (set! can-next #t)])]
          [(char=? c #\.)
           (when (not has-dot)
             (set! has-dot #t)
             (set! can-next #t))])
        (when can-next
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

    (define words null)
    (define add-word
      (if reverse?
          (λ (type word)
            (set! words (cons (s-word type word) words)))
          (λ (type word)
            (set! words (append words (cons (s-word type word) null))))))
    (define start (if reverse? (- len 1) 0))
    (define word-len 0)
    (define sub null)

    (let loop ()
      (let loop-non-han ()
        (define c (string-ref sentence start))
        (define is-han #f)
        (define out (box #f))
        (match c
          [(or (? char-numeric? _) #\- #\.)
           (set! start (read-number out start))
           (add-word 'number (unbox out))]
          [(? char-whitespace? _)
           (set! start (read-spaces out start))
           (add-word 'space (unbox out))]
          [(? en-word-char? _)
           (set! start (read-en-word out start))
           (add-word 'en (unbox out))]
          [(? wide-punctuation? _)
           (add-word 'wp (string c))
           (set! start ((if reverse? - +) start 1))]
          [else (set! is-han #t)])
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
            [(= word-len 1) (add-word 'text sub)]
            [(dict-has? sub) (add-word 'text sub)]
            [else
             (set! word-len (- word-len 1))
             (when (> word-len 0)
               (loop))]))
        (set! start ((if reverse? - +) start word-len)))
      (when (if reverse? (>= start 0) (< start len))
        (loop)))
    words)
  
  (define (single-count words)
    (count (λ (word) (= (string-length (s-word-text word)) 1))
           words))

  (cond
    [(non-empty-string? sentence)
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
          [(andmap (λ (x y)
                     (string=? (s-word-text x) (s-word-text y)))
                   f-words b-words) f-words]
          [else
           (define f-cnt (single-count f-words))
           (define b-cnt (single-count b-words))
           ; 否则，返回单字词较少的那个
           (if (>= f-cnt b-cnt)
               b-words
               f-words)])]
       [else
        (displayln f-words)
        (displayln b-words)
        ; 否则，返回词数较少的那个
        (if (> f-total b-total)
            b-words
            f-words)])]
    [else null]))