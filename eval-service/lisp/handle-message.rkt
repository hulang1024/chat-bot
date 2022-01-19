#lang racket
(require "eval.rkt"
         "../../chat/message/main.rkt"
         "../../chat/contact/message-send.rkt")

(provide handle-message
         execute-program)


(define (handle-message event add-message)
  (define source-message (send event get-message))
  (define m-str (string-trim (send source-message content-to-string)))
  (define expr-str #f)
  (define has-quote-reply? #f)
  (cond
    [(regexp-match? #rx"^\\s*#rkt.+" m-str)
     (set! expr-str (string-trim (substring m-str 4)))
     (set! has-quote-reply? #t)]
    [(regexp-match? #rx"^\\s*!rkt.+" m-str)
     (set! expr-str (string-trim (substring m-str 4)))
     (set! has-quote-reply? #f)]
    ; 匹配一对括号。不包括[]，因为会和消息中的"[图片]"等冲突。
    [(regexp-match? #rx"^\\s*[（\\(].+[\\)）]" m-str)
     (set! expr-str (string-trim m-str))])
  (if (and expr-str (not (string=? expr-str "")))
      (execute-program (send event get-subject)
                       (send event get-sender)
                       (send event get-message)
                       add-message expr-str has-quote-reply?)
      #f))


(define (execute-program subject sender source-message add-message expr has-quote-reply? [quiet-fail? #f])
  (handle-api-result (eval-program expr "global" sender)
                     expr
                     subject
                     source-message
                     add-message
                     has-quote-reply?
                     quiet-fail?))


(define (handle-api-result api-result expr subject source-message add-message has-quote-reply? quiet-fail?)
  (define ok? (send api-result ok?))
  (define output (send api-result get-output))
  (define length-good? (if ok? (output-length-good? output) #f))
  
  (cond
    [(and ok? length-good?)
     (when (not (null? output))
       (define mcb (new message-chain-builder%))
       (define add-message (create-add-message mcb))
       (define (convert-message items)
         (for-each
          (λ (item)
            (add-message
             (match (hash-ref item 'type "text")
               ["image" (new image-message%
                             [path (hash-ref item 'path)]
                             [url (hash-ref item 'url)])]
               ["audio" (new voice-message%
                             [path (hash-ref item 'path)]
                             [url (hash-ref item 'url)])]
               [else (new mirai-code-message% [code (hash-ref item 'content)])])))
          items))
       (when (and source-message has-quote-reply?)
         (add-message (make-quote-reply source-message)))
       (convert-message output)
       (define message-chain (send mcb build))
       (message-receipt-promise-then
        (send subject send-message (send message-chain trim))
        (λ (_)
          (for-each
           (λ (m)
             (when (and (or (is-a? m image-message%) (is-a? m voice-message%))
                        (path-string? (send m get-path)))
               (delete-file (send m get-path))))
           (send message-chain to-list)))))]
    [(and ok? (not length-good?))
     (add-message "消息太长啦")]
    [quiet-fail? #f]
    [else
     (define error (send api-result get-error))
     (define exn-data (send api-result get-data))
     (add-message "🎈")
     (cond
       [(hash? exn-data)
        (error-handler expr exn-data error add-message)]
       [else (add-message error)])
     #f]))


(define (output-length-good? output)
  (define text-line-max 61)
  (define image-max 11)
  (define audio-max 5)

  (define text-line-count 0)
  (define image-count 0)
  (define audio-count 0)
  (for-each
   (λ (item)
     (match (hash-ref item 'type "text")
       ["text"
        (define content (hash-ref item 'content))
        (set! text-line-count (+ text-line-count (length (string-split content "\n"))))]
       ["image"
        (set! image-count (+ image-count 1))]
       ["audio"
        (set! audio-count (+ audio-count 1))]))
   output)
  (and (< text-line-count text-line-max)
       (< image-count image-max)
       (< audio-count audio-max)))


(define (error-handler expr exn-data error add-message)
  (define exn-type (hash-ref exn-data 'type))
  (define (guess-wide-bracket text)
    (cond
      [(regexp-match? #rx"[（）]" text)
       "\n是不是把英文括号`()`打成中文括号`（）`了？"]
      [else ""]))
  
  (case exn-type
    [("read")
     (cond
       [(string-contains? error "expected a `)` to close `(`")
        (add-message (string-append "少了右括号？" (guess-wide-bracket expr)))]
       [else
        (add-message (string-append "读取阶段错误\n" error))])]
    [("syntax")
     (add-message (string-append "语法错误\n" error))]
    [("variable")
     (define id (hash-ref exn-data 'id))
     (add-message (string-append (format "【~a】未定义" id) (guess-wide-bracket expr)))]
    [("out-of-memory")
     (add-message "内存不够了")]
    [else
     (add-message error)]))