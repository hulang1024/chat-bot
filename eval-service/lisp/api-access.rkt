#lang racket
(require racket/match
         net/url
         json
         "config.rkt")

(provide perform-api
         api-result%)


(define api-result%
  (class object%
    (super-new)
    (init-field code data output value error)

    (define/public (get-code) code)
    (define/public (get-data) data)
    (define/public (get-output) output)
    (define/public (get-value) value)
    (define/public (get-error) error)
    (define/public (set-error text) (set! error text))

    (define/public (ok?) (= code 0))))


(define (perform-api path params)
  (set! params (hash-copy params))
  (hash-set! params 'path path)

  (define-values (status headers in)
    (http-sendrecv/url (string->url (format "http://~a" host))
                       #:method #"POST"
                       #:data (jsexpr->string params)))

  (define api-result (parser-api-result in))
  api-result)


(define (parser-api-result in)
  (define jsexpr (string->jsexpr (port->string in)))
  (define (get-value key [def #f])
    (hash-ref jsexpr key def))
  (new api-result%
       [code (get-value 'code)]
       [data (get-value 'data)]
       [output (get-value 'output "")]
       [value (get-value 'value)]
       [error (get-value 'error "")]))