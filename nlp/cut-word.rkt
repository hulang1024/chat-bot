#lang racket
(require "dict.rkt") 

(provide (rename-out (cut-words/mm cut-words))
         print/join)
         

(define (print/join words)
  (displayln (string-join words " / ")))


(define (cut-words/mm sentence)
  (set! sentence (string-trim sentence))
  (define len (string-length sentence))
  
  (define (fmm)
    (define words null)
    (define (add-word word)
      (set! words (append words (cons word null))))
    (define rest-start 0)
    (define word-len 0)
    (define sub null)
    (let loop ()
      (set! word-len (min dict-max-word-length (- len rest-start)))
      (let loop ()
        (set! sub (substring sentence rest-start (+ rest-start word-len)))
        (cond
          [(= word-len 1) (add-word sub)]
          [(dict-has? sub) (add-word sub)]
          [else
           (set! word-len (- word-len 1))
           (when (> word-len 0)
             (loop))]))
      (set! rest-start (+ rest-start word-len))
      (when (< rest-start len)
        (loop)))
    words)
	
  (define (bmm)
    (define words null)
    (define (add-word word)
      (set! words (cons word words)))
    (define rest-end len)
    (define word-len 0)
    (define sub null)
    (let loop ()
      (set! word-len (min dict-max-word-length rest-end))
      (let loop ()
        (set! sub (substring sentence (- rest-end word-len) rest-end))
        (cond
          [(= word-len 1) (add-word sub)]
          [(dict-has? sub) (add-word sub)]
          [else
           (set! word-len (- word-len 1))
           (when (> word-len 0)
             (loop))]))
      (set! rest-end (- rest-end word-len))
      (when (> rest-end 0)
        (loop)))
    words)

  (define f-words (fmm))
  (define b-words (bmm))
  (define (single-count words)
    (count (λ (word)
             (= (string-length word) 1))
           words))
  (define f-total (length f-words))
  (define b-total (length b-words))
  (define words
    (cond
      [(= f-total b-total)
       (cond
         [(andmap string=? f-words b-words) f-words]
         [else
          (define f-cnt (single-count f-words))
          (define b-cnt (single-count b-words))
          (if (>= f-cnt b-cnt)
              b-words
              f-words)])]
      [else
       (if (> f-total b-total)
           b-words
           f-words)]))
  (parse-words words))


(define (parse-words words)
  (define count (length words))
  (define result null)
  (define acc-word-chars null)
  
  (define (add-word word)
    (set! result (append result (cons word null))))

  (define (acc-word-char c)
    (set! acc-word-chars (append acc-word-chars (cons c null))))
  
  (define (add-acc-word)
    (unless (null? acc-word-chars)
      (add-word (list->string acc-word-chars))
      (set! acc-word-chars null)))
  
  (define prev-is-digit #f)
  (let loop ([i 0])
    (define word (list-ref words i))
    (cond
      [(= (string-length word) 1)
       (define c (string-ref word 0))
       (define n (char->integer c))
       (cond
         [(<= 48 n 57)
          (set! prev-is-digit #t)
          (acc-word-char c)]
         [(and (or (char=? c #\:) (char=? c #\：)) prev-is-digit)
          (acc-word-char #\:)
          (set! prev-is-digit #f)]
         [else
          (add-acc-word)
          (add-word word)
          (set! prev-is-digit #f)])]
      [else
       (add-acc-word)
       (add-word word)
       (set! prev-is-digit #f)])
    (when (< i (- count 1))
      (loop (+ i 1))))
  (add-acc-word)
  result)