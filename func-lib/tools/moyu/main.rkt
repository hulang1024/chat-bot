#lang racket
(require 2htdp/image
         (only-in racket/draw read-bitmap bitmap%)
         (only-in mrlib/image-core render-image)
         math/base
         net/url
         "config.rkt"
         "../../../user/user-manager.rkt"
         "moyu-image.rkt"
         "fishing.rkt"
         "fish-manager.rkt"
         "../pic.rkt"
         "../../../chat/message/main.rkt"
         "../../../chat/contact/message-send.rkt")

(provide moyu
         moyu-my-stat
         moyu-clear-my-fish-basket
         moyu-ranking
         moyu-help)


(define action-delay 10000)
(define fishing-game (new fishing-game%))

(define (moyu event)
  (define subject (send event get-subject))
  (define sender (send event get-sender))
  (thread
   (λ ()
     (define start (current-inexact-milliseconds))
     (define fish (send fishing-game fishing sender))
     (define (send-fish-image fish)
       (define ((draw-fish-image fish) moyu-bitmap)
         (define moyu-dc (send moyu-bitmap make-dc))
         (define canvas-w 480)
         (define canvas-h 278)

         (define cover-bitmap 
           (cond
             [fish
              (define fish-id (s-fish-id fish))
              (define fish-image-path (get-fish-image-path-by-id fish-id))
              (read-bitmap fish-image-path)]
             [else
              (define category (if (= (random-integer 0 2) 0) "mm" "scenery"))
              (define url (get-random-pic-url category))
              (cond
                [url
                 (define-values (status headers in)
                   (http-sendrecv/url (string->url url)))
                 (make-object bitmap% in)]
                [else #f])]))

         (when cover-bitmap
           (send moyu-dc draw-bitmap-section-smooth cover-bitmap
                 0 0 canvas-w canvas-h 0 0
                 (send cover-bitmap get-width)
                 (send cover-bitmap get-height)))

         (define banner-bitmap (read-bitmap (build-path work-path "banner.png")))
         (send moyu-dc draw-bitmap-section-smooth banner-bitmap
               0 0 canvas-w canvas-h 0 0
               (send banner-bitmap get-width)
               (send banner-bitmap get-height))
         ; 摸鱼结果信息
         (define base-x 41)
         (define x base-x)
         (define base-y 290)
         (define nickname-text (text/font (send sender get-nickname)
                                          16 "blue" "FZLanTingHeiS-R-GB" 'modern 'normal 'normal #f))
         (render-image nickname-text moyu-dc x base-y)
         (set! x (+ x (image-width nickname-text)))
         
         (cond
           [fish
            (define verb-num-text (text/font "摸到了一条"
                                             16 "black" "FZLanTingHeiS-R-GB" 'modern 'normal 'normal #f))
            (render-image verb-num-text moyu-dc x base-y)
            (set! x (+ x (image-width verb-num-text)))
              
            (define kg-text (text/font (format "~a公斤" (~r (s-fished-fish-weight fish) #:precision 2))
                                       16 "orange" "FZLanTingHeiS-R-GB" 'modern 'normal 'normal #f))
            (render-image kg-text moyu-dc x base-y)
            (set! x (+ x (image-width kg-text)))
                            
            (define de-text (text/font "的" 16 "black" "FZLanTingHeiS-R-GB" 'modern 'normal 'normal #f))
            (render-image de-text moyu-dc x base-y)
            (set! x (+ x (image-width de-text)))
                            
            (define fish-name-text (text/font (format "~a。" (s-fish-name fish))
                                              16 "red" "FZLanTingHeiS-R-GB" 'modern 'normal 'normal #f))
            (render-image fish-name-text moyu-dc x base-y)]
           [else
            (define info (text/font "本次未摸到鱼。"
                                    16 "black" "FZLanTingHeiS-R-GB" 'modern 'normal 'normal #f))
            (render-image info moyu-dc x base-y)]))
       (when (> action-delay 1000)
         (send subject send-message "请稍等"))
       (define path (make-moyu-image (draw-fish-image fish)))
       (message-receipt-promise-then
        (send subject send-message (new image-message% [path path]))
        (λ (_)
          (set! action-delay (- (current-inexact-milliseconds) start)))))
     (cond
       [fish
        (case fish
          [(2) (send subject send-message "放入鱼护失败，原因：鱼护满了。")]
          [(3) (send subject send-message "鱼护满了。")]
          [else
           (send-fish-image fish)])]
       [else
        (send-fish-image #f)])
     (user-manager:add-or-update sender))))


(define (moyu-my-stat event)
  (thread
   (λ ()
     (define subject (send event get-subject))
     (define sender (send event get-sender))
     (define mcb (new message-chain-builder%))
     (define add-message (create-add-message mcb))
     (define image-path (send fishing-game stat-user subject sender add-message))
     (send subject send-message (send mcb build)))))

(define (moyu-clear-my-fish-basket event add-message)
  (define subject (send event get-subject))
  (define sender (send event get-sender))
  (send fishing-game clear-fish-basket subject sender add-message))


(define (moyu-ranking by event add-message)
  (send fishing-game ranking by 50 add-message))


(define (moyu-help event add-message)
  (add-message "摸鱼帮助：\n 摸鱼\n 我的鱼\n 清空我的鱼\n 摸鱼排名\n 摸鱼排名按条数\n 摸鱼排名按重量"))