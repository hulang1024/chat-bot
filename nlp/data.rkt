#lang racket
(require 2htdp/batch-io
         "config.rkt")

(provide read-data-file)


(define name-to-data (make-hash))

(define (read-data-file filename)
  (define ext (filename-extension filename))
  (define name (if ext (substring filename 0
                                  (- (string-length filename)
                                     (+ (bytes-length ext) 1)))
                   filename))
  (cond
    [(hash-has-key? name-to-data name) (hash-ref name-to-data name)]
    [else
     (define lines (read-lines (string-append data-path filename)))
     (hash-set! name-to-data name lines)
     lines]))