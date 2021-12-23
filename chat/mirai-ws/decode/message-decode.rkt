#lang racket
(require racket/match
         "../../message/main.rkt")

(provide decode-message-chain)


(define (decode-message-chain js-array-expr)
  (define message-chain-builder (new message-chain-builder%))
  (for ([jsexpr js-array-expr])
    (define message (decode-message jsexpr))
    (when message
      (send message-chain-builder add message)))
  (send message-chain-builder build))


(define (decode-message jsexpr)
  (define type (hash-ref jsexpr 'type))
  (match type
    ["Source" (decode-source jsexpr)]
    ["Plain" (decode-plain jsexpr)]
    ["Quote" (decode-quote jsexpr)]
    ["At" (decode-at jsexpr)]
    ["AtAll" (decode-at-all jsexpr)]
    ["Face" (decode-face jsexpr)]
    ["Image" (decode-image jsexpr)]
    ["FlashImage" (decode-flash-image jsexpr)]
    ["Voice" (decode-voice-message jsexpr)]
    [_ #f]))


(define (decode-source jsexpr)
  (match-define (hash-table ('id id)
                            ('time time)) jsexpr)
  (new source%
       [id id]
       [time time]))


(define (decode-plain jsexpr)
  (match-define (hash-table ('text text)) jsexpr)
  (new plain% [text text]))


(define (decode-quote jsexpr)
  (match-define (hash-table ('id id)
                            ('groupId group-id)
                            ('senderId sender-id)
                            ('targetId target-id)
                            ('origin origin-jsexpr)) jsexpr)
  (define origin (decode-message-chain origin-jsexpr))
  (new quote%
       [id id]
       [group-id group-id]
       [sender-id sender-id]
       [target-id target-id]
       [origin origin]))


(define (decode-at jsexpr)
  (match-define (hash-table ('target target)
                            ('display display)) jsexpr)
  (new at% [target target]))


(define (decode-at-all jsexpr)
  (new at-all%))


(define (decode-face jsexpr)
  (match-define (hash-table ('faceId id)
                            ('name name)) jsexpr)
  (new face%
       [id id]
       [name name]))


(define (decode-image jsexpr)
  (match-define (hash-table ('imageId id)
                            ('url url)
                            ('path path)) jsexpr)
  (new image-message%
       [id id]
       [url url]
       [path path]))


(define (decode-flash-image jsexpr)
  (match-define (hash-table ('imageId id)
                            ('url url)
                            ('path path)) jsexpr)
  (new flash-image-message%
       [id id]
       [url url]
       [path path]))


(define (decode-voice-message jsexpr)
  (match-define (hash-table ('voiceId id)
                            ('url url)
                            ('path path)
                            ('length length)) jsexpr)
  (new voice-message%
       [id id]
       [url url]
       [path path]
       [length length]))