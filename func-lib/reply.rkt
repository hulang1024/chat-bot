#lang racket
(require math/base
         net/url
         json
         "../config.rkt"
         "../chat/message/main.rkt")

(provide reply-talk
         (prefix-out reply: is-time?)
         (prefix-out reply: active-mode)
         (prefix-out reply: set-active-mode))

(define active-mode "normal")
(define last-reply-time 0)

(define (reply-talk msg add-message)
  (set! last-reply-time (current-inexact-milliseconds))
  (with-handlers ([(const #t) (λ (v) #f)])
    (define-values (status headers in)
      (http-sendrecv/url
       (string->url (format "http://api.qingyunke.com/api.php?key=free&appid=0&msg=~A"
                            msg))))
    (define api-result (string->jsexpr (port->string in)))
    (define res (hash-ref api-result 'content))

    (define str (string-replace res "菲菲" bot-nickname #:all? #t))

    (let ([len (string-length str)]
          [res str])
      (let loop ([i 0])
        (when (< i len)
          (define c (string-ref str i))
          (define added? #f)
          (when (char=? c #\{)
            (cond
              [(string=? (substring str (+ i 1) (+ i 3)) "br")
               (add-message "\n")
               (set! i (+ i 3))
               (set! added? #t)]
              [(string=? (substring str (+ i 1) (+ i 6)) "face:")
               (define start (+ i 6))
               (define end
                 (let util-close ([j start])
                   (if (and (< j len) (not (char=? (string-ref str j) #\})))
                       (util-close (+ j 1))
                       j)))
               (when (< start end)
                 (set! i end)
                 (add-message (face-from-id (string->number (substring str start end)))))
               (set! added? #t)]))
          (when (not added?)
            (add-message (string c)))
          (loop (+ i 1)))))))

(define (is-time?)
  (define max
    (case active-mode
      [("reply") 1]
      [("active") 5]
      [("normal") 20]
      [("quiet") 0]))
  (and (not (= max 0))
       (> (- (current-inexact-milliseconds) last-reply-time) 1000)
       (or (= max 1) (= (random-integer 0 max) 1))))

(define (set-active-mode mode)
  (set! active-mode mode))