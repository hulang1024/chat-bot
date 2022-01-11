#lang racket
(require racket/draw
         "config.rkt"
         "moyu-image.rkt"
         "fishing.rkt"
         "fish-manager.rkt"
         "../../../chat/message/main.rkt"
         "../../../chat/contact/message-send.rkt")

(provide moyu
         moyu-help)


(define action-delay 10000)
(define fishing-game (new fishing-game%))

(define (moyu event)
  (define subject (send event get-subject))
  (define sender (send event get-sender))
  (when (> action-delay 300)
    (send subject send-message "请稍等"))

  (thread
   (λ ()
     (define start (current-inexact-milliseconds))
     (define mcb (new message-chain-builder%))
     (define add-message (create-add-message mcb))
     (define fish (send fishing-game fishing sender add-message))
     (define path (make-moyu-image (draw-fish-image fish)))
     (add-message (new image-message% [path (path->string path)]))
     (message-receipt-promise-then
      (send subject send-message (send mcb build))
      (λ (_)
        (set! action-delay (- (current-inexact-milliseconds) start)))))))


(define ((draw-fish-image fish) moyu-bitmap)
  (define moyu-dc (send moyu-bitmap make-dc))
  (define canvas-w 480)
  (define canvas-h 278)

  (define fish-id (s-fish-id fish))
  (define fish-image-path (get-fish-image-path-by-id fish-id))
  (define fish-bitmap (read-bitmap fish-image-path))
  (send moyu-dc draw-bitmap-section-smooth fish-bitmap
        0 0 canvas-w canvas-h 0 0
        (send fish-bitmap get-width)
        (send fish-bitmap get-height))

  (define banner-bitmap (read-bitmap (build-path work-path "banner.png")))
  (send moyu-dc draw-bitmap-section-smooth banner-bitmap
        0 0 canvas-w canvas-h 0 0
        (send banner-bitmap get-width)
        (send banner-bitmap get-height)))


(define (moyu-help event add-message)
  (add-message "摸鱼帮助：\n 摸鱼\n 我的鱼\n 清空我的鱼\n 摸鱼排名\n 摸鱼排名按条数\n 摸鱼排名按重量"))