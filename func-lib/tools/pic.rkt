#lang racket
(require math/base
         net/url
         "../utils/http-util.rkt"
         "../utils/random.rkt"
         "../../chat/message/message.rkt")

(provide get-random-pic)


(define (get-random-pic add-message)
  (add-message (new image-message% [url (get-random-pic-api-url)])))


(define api-urls '("https://api.nmb.show/1985acg.php"))
(define redirect-api-urls '("https://cdn.seovx.com/?mom=302"))

(define (get-random-pic-api-url)
  (cond
    [(= (random-natural 2) 0)
     (define api-url (list-random-ref redirect-api-urls))
     (get-image-redirect-url api-url)]
    [else (list-random-ref api-urls)]))


(define (get-image-redirect-url url)
  (define-values (status headers in)
    (http-sendrecv/url (string->url url)))
  (cond
    [(string-contains? (bytes->string/utf-8 status) "302")
     (define location (get-header-value #"Location" headers))
     location]
    [else (error (format "[~a]的请求未响应重定向" url))]))

