#lang racket
(require racket/date
         2htdp/batch-io
         "config.rkt"
         "./remind-manager.rkt"
         "../../../nlp/tagging.rkt"
         "../../../nlp/time.rkt"
         "../../../timer.rkt"
         "../../../chat/message/main.rkt"
         "../../../chat/contact/group.rkt"
         "../../../chat/contact/friend.rkt"
         "../../../eval-service/lisp/handle-message.rkt")

(provide remind-load
         create-remind
         cancel-remind
         my-reminds
         my-created-reminds
         all-reminds
         remind-help
         remind-parse-args)


(define remind-timers (make-hash))

(define (create-remind event remind add-message)
  (define now (current-date))
  (cond
    [(> (s-remind-time remind) (date->seconds now))
     (define subject (send event get-subject))
     (remind-mgr:save remind)
     (set-remind subject remind #:event event)
     (add-message (make-quote-reply (send event get-message)))
     (define sender (send event get-sender))
     (define target-uid (s-remind-target-uid remind))
     (define self? (= (send sender get-id) target-uid))
     (when (and (is-a? subject group%) self?)
       (add-message (new at% [target target-uid])))
     (add-message (format " 好的，将在~a提醒~a~a"
                          (date-seconds->short-string (s-remind-time remind) now)
                          (cond
                            [(= target-uid 0) "大家"]
                            [self? "你"]
                            [else "他"])
                          (s-remind-content remind)))]
    [else
     (add-message "时间已过")]))


(define (cancel-remind sender remind-id add-message)
  (define remind (remind-mgr:query-by-id remind-id))
  (cond
    [(false? remind)
     (add-message (format "没有id为~a提醒哦" remind-id))]
    [(= (s-remind-create-uid remind) (send sender get-id))
     (remind-mgr:delete remind-id)
     (cancel-timer remind-id)
     (add-message "已经取消提醒啦")]
    [else
     (add-message "这不是你的提醒，无权操作")]))


(define (remind-load bot)
  (define reminds (remind-mgr:query-all))
  (for-each
   (λ (remind)
     (define subject (if (> (s-remind-target-group-id remind) 0)
                         (new group% [bot bot] [id (s-remind-target-group-id remind)])
                         (new friend% [bot bot] [id (s-remind-target-uid remind)])))
     (set-remind subject remind))
   reminds))


(define (cancel-timer remind-id)
  (cond
    [(hash-has-key? remind-timers remind-id)
     (define timer (hash-ref remind-timers remind-id))
     (send timer stop)
     (hash-remove! remind-timers remind-id)
     #t]
    [else #f]))


(define (set-remind subject remind #:event [event #f])
  (define (handle-timeout)
    (cancel-timer (s-remind-id remind))
    (remind-mgr:delete (s-remind-id remind))
    
    (define (repeat-thing repeat-current)
      (define mcb (new message-chain-builder%))
      (define add-message (create-add-message mcb))
      (define source-message (if event (send event get-message) #f))
      (define content (s-remind-content remind))
      (define target-everyone? (= (s-remind-target-uid remind) 0))
      (when (and (and (= repeat-current 1) source-message)
                 (not target-everyone?))
        (add-message (make-quote-reply source-message)))
      (when (and (is-a? subject group%)
                 (not target-everyone?))
        (add-message (new at% [target (s-remind-target-uid remind)])))
      (when (and (or (= repeat-current 1) (string=? content ""))
                 (not target-everyone?))
        (add-message " 时间到了"))
      (when (non-empty-string? content)
        (define code-prefix "并执行")
        (displayln content)
        (cond
          [(string-prefix? content code-prefix)
           (when (= repeat-current 1)
             (define mc (send mcb build))
             (when (not (send mc empty?))
               (send subject send-message mc)))
           (define expr (substring content (string-length code-prefix)))
           (define ok? (execute-program subject #f source-message add-message expr #f))
           (when (not ok?)
             (send subject send-message (send mcb build))
             (send repeat-timer cancel))]
          [else
           (add-message (string-append " " content))
           (send subject send-message (send mcb build))])))
    (define repeat-timer (new repeat-timer%
                       [on-repeat repeat-thing]
                       [times (s-remind-repeat-times remind)]
                       [seconds 1]))
    (send repeat-timer start))

  (define timer (new timer%
                     [time (s-remind-time remind)]
                     [on-timeout handle-timeout]))
  (hash-set! remind-timers (s-remind-id remind) timer)
  (send timer start))


(define (my-reminds event add-message)
  (define sender (send event get-sender))
  (build-remind-list-message event
                             (remind-mgr:query-by-target-uid (send sender get-id))
                             add-message
                             #:title "给你的提醒"
                             #:no-data "没有给你的提醒"))


(define (my-created-reminds event add-message)
  (define sender (send event get-sender))
  (build-remind-list-message event
                             (remind-mgr:query-by-create-uid (send sender get-id))
                             add-message
                             #:title "你定的提醒"
                             #:no-data "你定的提醒列表空"))


(define (all-reminds event add-message)
  (define reminds (remind-mgr:query-all))
  (define now (current-date))
  (cond
    [(null? reminds)
     (add-message "无提醒")]
    [else
     (define now (current-date))
     (add-message "所有提醒\n")
     (add-message "id  提醒时间 提醒对象 提醒群组 重复次数 内容 创建者 创建时间\n")
     (for-each
      (λ (remind)
        (add-message (format "~a  ~a ~a ~a ~a ~a ~a ~a\n"
                             (s-remind-id remind)
                             (date-seconds->short-string (s-remind-time remind) now)
                             (s-remind-target-uid remind)
                             (s-remind-target-group-id remind)
                             (s-remind-repeat-times remind)
                             (s-remind-content remind)
                             (s-remind-create-uid remind)
                             (s-remind-create-at remind))))
      reminds)]))


(define (build-remind-list-message event reminds add-message
                                   #:title title #:no-data no-data)
  (define sender (send event get-sender))
  (define subject (send event get-subject))
  (when (is-a? subject group%)
    (add-message (new at% [target (send sender get-id)])))
  (cond
    [(null? reminds)
     (add-message (string-append " " no-data))]
    [else
     (define now (current-date))
     (add-message (string-append " " title "\n"))
     (for-each
      (λ (remind)
        (define self? (= (send sender get-id) (s-remind-target-uid remind)))
        (add-message (format " ~a  在~a 提醒~a ~a\n"
                             (s-remind-id remind)
                             (date-seconds->short-string (s-remind-time remind) now)
                             (if self? "你" (format "QQ~a" (s-remind-target-uid remind)))
                             (s-remind-content remind))))
      reminds)]))


(define ((remind-parse-args event) words)
  (define subject (send event get-subject))
  (define sender (send event get-sender))
  
  (define (make-args target-uid time-word content [times 3] [code? #f])
    (define ret-date (time-word->date time-word))
    (cond
      [ret-date
       (define message (send event get-message))
       (define message-string (send message content-to-string))
       (define content-string (string-trim
                               (substring message-string
                                          (tagged-word/text-start (first content)))))
       (list event
             (s-remind (remind-mgr:generate-id)
                       (date->seconds ret-date)
                       target-uid
                       (if (is-a? subject group%) (send subject get-id) 0)
                       (min 10 times)
                       (if code? (string-append "并执行" content-string) content-string)
                       (send sender get-id)
                       null))]
      [else
       (define mcb (new message-chain-builder%))
       (define add-message (create-add-message mcb))
       (add-message (make-quote-reply (send event get-message)))
       (add-message "不能提醒这个时间哦")
       (send subject send-message (send mcb build))
       (void)]))
  (match words
    [(or (list (tagged-word 'time time)
               (tagged-word 'text (or "叫" "提醒"))
               (tagged-word 'text "我")
               content ...)
         (list (tagged-word 'text (or "叫" "提醒"))
               (tagged-word 'text "我")
               (tagged-word 'time time)
               content ...))
     (make-args (send sender get-id) time content)]
    [(list (tagged-word 'time time)
           (tagged-word 'text (or "叫" "提醒"))
           (tagged-word 'text "大家")
           content ...)
     (make-args 0 time content)]
    [(list (tagged-word 'time time)
           (tagged-word 'text "执行")
           (tagged-word 'number times)
           (tagged-word 'text "次")
           content ...)
     (make-args 0 time content times #t)]
    [(list (tagged-word 'time time)
           (tagged-word 'text (or "叫" "提醒"))
           (tagged-word 'wp "@")
           (tagged-word 'number other-uid)
           content ...)
     (make-args other-uid time content)]
    [(list (tagged-word 'time time)
           (tagged-word 'text (or "叫" "提醒"))
           (tagged-word 'wp "@")
           content ...)
     (define mcb (new message-chain-builder%))
     (define add-message (create-add-message mcb))
     (add-message (make-quote-reply (send event get-message)))
     (add-message (face-from-id 176))
     (add-message "我不知道是谁，请重新@加qq号")
     (send subject send-message (send mcb build))
     (void)]
    [else #f]))


(define (remind-help add-message)
  (add-message "你可以这样跟我说\n")
  (for-each
   (λ (stmt)
     (add-message (string-append "  " stmt "\n")))
   (list "3分钟后叫我喝水" "10秒后叫@<qq号>喝水" "8点提醒我改bug"
         "我的提醒" "我定的提醒" "取消提醒 <提醒id>")))