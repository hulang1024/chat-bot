#lang racket
(require "../utils/text-util.rkt")

(provide osu-menu
         osu-stat
         sender-bind-to-osu-user)


(module osu-api racket
  ; https://github.com/ppy/osu-api/wiki
  (require net/url
           net/uri-codec
           json)

  (#%provide get-user)


  (define base-url "osu.ppy.sh/api")
  (define api-key "8d81085d1374ea124c124283fe7612b7cb35dbd8")
  
  (define (get-user #:u u #:mode [mode 0])
    (get-json 'get_user `((u . ,u) (m . ,mode))))

  (define (get-json path params)
    (define query (alist->form-urlencoded
                   (map (λ (p)
                          (let ([k (car p)] [v (cdr p)])
                            (cons k (cond
                                      [(number? v) (number->string v)]
                                      [else v]))))
                        params)))
    (define url-str (format "https://~A/~A?k=~A&~A"
                            base-url
                            (symbol->string path)
                            api-key
                            query))
    (define url (string->url url-str))
    (define-values (status headers in)
      (http-sendrecv/url url))
    (string->jsexpr (port->string in))))

 
(require 'osu-api)

(define osu-user-manager%
  (class object%
    (super-new)

    (define osu-user-ids (make-hash))
    
    (define/public (get-users) osu-user-ids)
    
    (define/public (bind-user uid osu-uid)
      (hash-set! osu-user-ids uid osu-uid))

    (define/public (get-osu-user-id uid)
      (hash-ref osu-user-ids uid))

    (define/public (has-binding? uid)
      (hash-has-key? osu-user-ids uid))
    
    (define mode-name-hash
      (hash 0 "osu!"
            1 "Taiko"
            2 "CatchTheBeat"
            3 "osu!mania"))

    (define display-result-key-zh-hash
      (hash 'username "用户名"
            'country "国家"
            'pp_rank "PP排名"
            'pp_country_rank "PP国家排名"
            'pp_raw "PP"
            'accuracy "Acc"
            'playcount "游玩次数"
            'ranked_score "已排名分数"
            'total_score "总分数"
            'level "当前等级"
            'count300 "300x"
            'count100 "100x"
            'count50 "50x"
            'count_rank_ss "SS"
            'count_rank_s "S"
            'count_rank_a "A"
            'mode "模式"))
    
    (define display-orderedy-keys
      '(
        username
        country
        pp_rank pp_country_rank pp_raw
        accuracy
        playcount
        ranked_score
        total_score
        level
        count300 count100 count50
        count_rank_ss count_rank_s count_rank_a))

    (define/public (query-osu-user osu-uid mode display)
      (define result (get-user #:u osu-uid #:mode mode))
      (if (not (null? result))
          (let ([result (list-ref result 0)])
            (display (string-append "模式：" (hash-ref mode-name-hash mode) "\n"))
            (for ([key display-orderedy-keys])
              (when (and (hash-has-key? result key)
                         (hash-has-key? display-result-key-zh-hash key))
                (let ([title (hash-ref display-result-key-zh-hash key)]
                      [text (hash-ref result key)])
                  (display (string-append
                              title
                              (build-string (- 14 (text-width title)) (λ (n) #\space))
                              text
                              "\n"))))))
          #f))))
          

(define osu-user-manager (new osu-user-manager%))

(define mode-names '("osu" "taiko" "ctb" "mania"))

(define (osu-stat mode-name osu-uid sender-id add-message)
  (define mode (match mode-name ["osu" 0] ["taiko" 1] ["ctb" 2] ["mania" 3] [_ #f]))
  (cond
    [(not mode)
     (add-message (string-append "第一个参数只可以写以下文字：" (string-join mode-names " ")))]
    [(false? osu-uid)
     (if (send osu-user-manager has-binding? sender-id)
         (osu-stat mode-name
                   (send osu-user-manager get-osu-user-id sender-id)
                   sender-id
                   add-message)
         (add-message "请先发送:\n设置osu用户 你的osu用户名或id\n"))]
    [else 
     (define found (send osu-user-manager query-osu-user osu-uid mode add-message))
     (when (not found)
       (add-message "未找到osu用户 ~a\n" osu-uid))]))


(define (sender-bind-to-osu-user sender-id osu-uid add-message)
  (send osu-user-manager bind-user sender-id osu-uid)
  (add-message "设置成功\n"))


(define (osu-menu display)
  (display "osu命令菜单\n========\n")
  (define m-names `("" ,@(cdr mode-names)))
  (display "自己的分数统计\n")
  (display "\t设置osu用户 你的osu用户名或id\n")
  (for ([mode m-names])
    (display (format "\tosu-stat ~A\n" mode)))
  (display "别人的分数统计\n")
  (for ([mode m-names])
    (display (format "\tosu-stat ~A osu用户名或id\n" mode))))