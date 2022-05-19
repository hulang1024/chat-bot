#lang racket
(require net/url
         json)

(provide get-joke)


(define api-key "7e9401a58dc937d2d827a09fc39f37a3")

(define (get-joke add-message)
  (define (create-url)
    (string->url (format "~A?key=~A"
                         "https://v.juhe.cn/joke/randJoke.php"
                         api-key)))

  (define-values (status headers in)
    (http-sendrecv/url (create-url)))
  (define api-result (string->jsexpr (port->string in)))
  (define result (hash-ref api-result 'result))

  (add-message (if (list? result)
                   (hash-ref (list-ref result 0) 'content)
                   "请稍后重试")))