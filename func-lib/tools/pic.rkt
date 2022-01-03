#lang racket
(require math/base
         net/url
         "../utils/http-util.rkt"
         "../utils/random.rkt"
         "../../chat/message/main.rkt"
         "../../chat/contact/message-send.rkt")

(provide get-random-pic)


(define pic-api-info%
  (class object%
    (super-new)
    ; api-type: 'direct 'redirect
    (init-field category url type)

    (define/public (get-category) category)
    (define/public (get-url) url)
    (define/public (get-type) type)))

; 直接返回图片流的
(define (direct-api category url)
  (new pic-api-info%
       [category category]
       [url url]
       [type 'direct]))

; 需要一次重定向的
(define (redirect-api category url)
  (new pic-api-info%
       [category category]
       [url url]
       [type 'redirect]))

(define pic-apis
  (list (direct-api "anime" "https://api.nmb.show/1985acg.php")
        (redirect-api "anime" "https://api.ixiaowai.cn/api/api.php")
        (redirect-api "scenery" "https://api.ixiaowai.cn/gqapi/gqapi.php")
        (redirect-api "mm" "https://cdn.seovx.com/?mom=302")))
        

(define receipt-delay 10000)

(define (get-random-pic category event)
  (define api-info (list-random-ref
                    (if (string=? category "all")
                        pic-apis
                        (filter (λ (a) (string=? (send a get-category) category))
                                pic-apis))))
  (define url (send api-info get-url))
  (set! url
        (match (send api-info get-type)
          ['direct url]
          ['redirect
           (define r-url (get-image-redirect-url url))
           (if r-url
               r-url
               #f)]))

  (define mcb (new message-chain-builder%))
  (define add-message (create-add-message mcb))
  (cond
    [url
     (when (> receipt-delay 300)
       (send (send event get-subject) send-message "请稍等"))
     (add-message (new image-message% [url url]))]
    [else
     (add-message (face-from-id 270))
     (add-message "图片接口发生错误,请重试")])

  (define send-time (current-inexact-milliseconds))
  (define receipt
    (send (send event get-subject) send-message (send mcb build)))
  (message-receipt-promise-then
   receipt
   (λ (_)
     (set! receipt-delay (- (current-inexact-milliseconds) send-time)))))

(define (get-image-redirect-url url)
  (define-values (status headers in)
    (http-sendrecv/url (string->url url)))
  (cond
    [(string-contains? (bytes->string/utf-8 status) "302")
     (define location (get-header-value #"Location" headers))
     location]
    [else #f]))

