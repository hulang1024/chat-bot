#lang racket

(require "contact.rkt")

(provide user%)


(define user%
  (class contact%
    (super-new)

    (init-field nickname)))