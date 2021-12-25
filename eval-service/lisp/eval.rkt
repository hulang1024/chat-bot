#lang racket
(require json
         "api-access.rkt")

(provide eval-program)


(define (eval-program expr env-id sender)
  (define sender-info
    (if sender (hash 'id (send sender get-id)
                     'nickname (send sender get-nickname))
        (hash 'id 0)))

  (define params (hash 'expr expr
                       'env_id env-id
                       'sender sender-info))

  (define api-result (perform-api "eval" params))

  (when (and (not (send api-result ok?))
             (string=? (send api-result get-error) ""))
    (send api-result set-error "\uD83D\uDC7B程序执行请求好像发生异常了哦,请重试。"))

  api-result)