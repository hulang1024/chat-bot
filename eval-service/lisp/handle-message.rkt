#lang racket
(require "eval.rkt"
         "../../chat/message/main.rkt")

(provide handle-message)


(define (handle-message event add-message)
  (define m-str (string-trim (send (send event get-message) content-to-string)))
  (cond
    [(regexp-match #rx"^\\s*#rkt.+" m-str)
     (define expr-str (string-trim (substring m-str 4)))
     (if (not (string=? expr-str ""))
         (handle-api-result (eval-program expr-str
                                          "global"
                                          (send event get-sender))
                                          add-message)
         #f)]
    [else #f]))


(define (handle-api-result api-result add-message)
  (cond
    [(send api-result ok?)
     (define value (send api-result get-value))
     (define output (string-trim (send api-result get-output)))
     (define has-output (not (string=? output "")))
     (add-message output)
     (when (and value (or (not has-output)
                          (not (string=? value "#<void>"))))
       (when has-output
         (add-message "\n"))
       (add-message (string-trim value)))]
    [else
     (define error (send api-result get-error))
     (add-message (face-from-id 270))
     (add-message error)]))