#lang racket

(provide (all-defined-out))


(define message<%>
  (interface ()
    content-to-string))


(define single-message<%>
  (interface (message<%>)))


(define source%
  (class* object% (single-message<%>)
    (super-new)

    (init-field id time)
    
    (define/public (get-id) id)
    (define/public (get-time) time)
    
    (define/public (content-to-string) "")))


(define plain%
  (class* object% (single-message<%>)
    (super-new)

    (init-field text)

    (define/public (get-text) text)

    (define/public (content-to-string) text)))


(define quote%
  (class* object% (single-message<%>)
    (super-new)

    (init-field id
                origin
                [group-id #f]
                [sender-id #f]
                [target-id #f])
    
    (define/public (get-id) id)
    (define/public (get-group-id) group-id)
    (define/public (get-sender-id) sender-id)
    (define/public (get-target-id) target-id)
    (define/public (get-origin) origin)

    (define/public (content-to-string) "")))


(define at%
  (class* object% (single-message<%>)
    (super-new)

    (init-field target)

    (define/public (get-target) target)

    (define/public (content-to-string) "")))


(define at-all%
  (class* object% (single-message<%>)
    (super-new)

    (define/public (content-to-string) "")))


(define face%
  (class* object% (single-message<%>)
    (super-new)

    (init-field id name)

    (define/public (get-id) id)
    (define/public (get-name) name)

    (define/public (content-to-string) "")))


(define image-message%
  (class* object% (single-message<%>)
    (super-new)

    (init-field [id #f]
                [url #f]
                [path #f])
    
    (define/public (get-id) id)
    (define/public (get-url) url)
    (define/public (get-path) path)

    (define/public (content-to-string) "")))


(define flash-image-message%
  (class image-message%
    (super-new)

    (define/override (content-to-string) "")))


(define voice-message%
  (class* object% (single-message<%>)
    (super-new)

    (init-field [id #f]
                [url #f]
                [path #f]
                [length #f])
    
    (define/public (content-to-string) "")))


(define app-message%
  (class* object% (single-message<%>)
    (super-new)

    (init-field content)

    (define/public (get-content) content)

    (define/public (content-to-string) "")))


(define poke-message%
  (class* object% (single-message<%>)
    (super-new)

    (init-field name)

    (define/public (get-name) name)

    (define/public (content-to-string) "")))


(define dice-message%
  (class* object% (single-message<%>)
    (super-new)

    (init-field value)

    (define/public (get-value) value)

    (define/public (content-to-string) "")))


(define music-share%
  (class* object% (single-message<%>)
    (super-new)

    (init-field kind
                title
                jump-url
                picture-url
                music-url
                [summary ""]
                [brief ""])

    (define/public (get-kind) kind)
    (define/public (get-title) title)
    (define/public (get-jump-url) jump-url)
    (define/public (get-picture-url) picture-url)
    (define/public (get-music-url) music-url)
    (define/public (get-summary) summary)
    (define/public (get-brief) brief)
    
    (define/public (content-to-string) "")))