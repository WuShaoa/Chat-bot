#lang racket
(require net/url
         net/uri-codec
         json)
(provide get-chat-answer)

(define (get-chat-answer username userid query)
    (define APPID 'ufZY9s5A7osdM24)
    (define TOKEN 'dCtPyTN3S57GvV9AmPMii1gRKAmtkm)
;请求signature
    (define sign_url (format "https://openai.weixin.qq.com/openapi/sign/~a" TOKEN))

    (define-values (sign_status sign_headers sign_in)
        (http-sendrecv/url (string->url sign_url)
                     #:method #"POST"
                     #:data  (alist->form-urlencoded
                                (list (cons 'userid userid)
                                      (cons 'username username)))
                     #:headers (list "Content-Type: application/x-www-form-urlencoded")))
    
; 请求对话query
  
    (define signature (hash-ref (string->jsexpr (port->string sign_in))
                                'signature
                                #f))
    (define ans_url (format "https://openai.weixin.qq.com/openapi/aibot/~a" TOKEN))

    (define-values (status headers ans_in)
      (http-sendrecv/url (string->url ans_url)
                         #:method #"POST"
                         #:data  (alist->form-urlencoded
                                    (list (cons 'signature signature)
                                          (cons 'query query)))
                         #:headers (list "Content-Type: application/x-www-form-urlencoded")))

    (display (port->string ans_in))

    ;(define ans-dict (string->jsexpr (port->string ans_in)))
    ;                   
    ;(cond
    ;    ((string=? (hash-ref ans-dict 'answer_type #f) "music")
    ;        (string-append (hash-ref ans-dict 'answer "")
    ;                       "\n" 
    ;                       (hash-ref (car (hash-ref ans-dict 'msg '(#f #f))) 'music_url "")))
    ;    ((string=? (hash-ref ans-dict 'answer_type #f) "news")
    ;        (string-append (hash-ref ans-dict 'answer "")
    ;                       "\n" 
    ;                       (hash-ref (hash-ref ans-dict 'more_info #f) 'news_ans_detail "")))
    ;    (else
    ;        (hash-ref ans-dict 'answer #f)))
)