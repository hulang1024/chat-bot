#lang racket
(require racket/match
         json
         "../../message/main.rkt")

(provide encode-message-chain)


(define (encode-message-chain message)
  (define js-array-expr null)
  (for ([m (send message to-list)])
    (define jsexpr (encode-message m))
    (when jsexpr
      (set! js-array-expr (append js-array-expr (list jsexpr)))))
  js-array-expr)


(define (encode-message message)
  (define encode
    (match (object-name message)
      ['object:plain% encode-plain]
      ['object:quote% #f] ; 不使用，已在发送消息命令的参数中
      ['object:at% encode-at]
      ['object:at-all% encode-at-all]
      ['object:face% encode-face]
      ['object:image-message% encode-image-message]
      ['object:flash-image-message% encode-flash-image-message]
      ['object:voice-message% encode-voice-message]
      ['object:music-share% encode-music-share]
      ['object:poke-message% encode-poke-message]
      ['object:app-message% encode-app-message]
      ['object:dice-message% encode-dice-message]
      ['object:mirai-code-message% encode-mirai-code-message]
      [else #f]))
  (if encode (encode message) #f))


(define (encode-plain m)
  (hash 'type "Plain"
        'text (send m get-text)))


(define (encode-quote m)
  (hash 'type "Quote"
        'id (send m get-id)
        'groupId (send m get-group-id)
        'senderId (send m get-sender-id)
        'targetId (send m get-target-id)
        'origin (encode-message-chain (send m get-origin))))


(define (encode-at m)
  (hash 'type "At"
        'target (send m get-target)))


(define (encode-at-all m)
  (hash 'type "AtAll"))


(define (encode-face m)
  (hash 'type "Face"
        'faceId (send m get-id)
        'name (send m get-name)))


(define (encode-image-message m)
  (hash 'type "Image"
        'imageId (null-ifnot (send m get-id))
        'url (encode-url (null-ifnot (send m get-url)))
        'path (encode-path (send m get-path))))


(define (encode-flash-image-message m)
  (hash 'type "FlashImage"
        'imageId (null-ifnot (send m get-id))
        'url (encode-url (null-ifnot (send m get-url)))
        'path (encode-path (send m get-path))))


(define (encode-voice-message m)
  (hash 'type "Voice"
        'voiceId (null-ifnot (send m get-id))
        'url (encode-url (null-ifnot (send m get-url)))
        'path (encode-path (send m get-path))))


(define (encode-poke-message m)
  (hash 'type "Poke"
        'name (send m get-name)))


(define (encode-app-message m)
  (hash 'type "App"
        'content (send m get-content)))


(define (encode-dice-message m)
  (hash 'type "Dice"
        'value (send m get-value)))


(define (encode-music-share m)
  (hash 'type "MusicShare"
        'kind (send m get-kind)
        'title (send m get-title)
        'jump-url (encode-url (send m get-jump-url))
        'picture-url (encode-url (send m get-picture-url))
        'music-url (encode-url (send m get-music-url))
        'summary (send m get-summary)
        'brief (send m get-brief)))


(define (encode-mirai-code-message m)
  (hash 'type "MiraiCode"
        'code (send m get-code)))

(define (null-ifnot v)
  (if v v (json-null)))

(define (encode-path v)
  (if (path-string? v) (path->string v) (null-ifnot v)))

(define (encode-url url)
  (if (string? url) (string-replace url " " "%20") url))