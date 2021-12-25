#lang racket

(require "contact.rkt")

(provide user%)


(define user%
  (class contact%
    (super-new)

    (abstract get-nickname)))