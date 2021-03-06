#lang racket
(require racket/match
         json
         "../message/main.rkt"
         "../mirai-ws/client.rkt"
         "../mirai-ws/command.rkt"
         "../mirai-ws/command-response.rkt"
         "../mirai-ws/encode/message-encode.rkt")

(provide send-group-message
         send-friend-message
         message-receipt-promise-then)


(define (message-receipt-promise-then promise proc)
  (send promise then proc))

(define (send-group-message #:group group
                            #:message message)
  (send-group-or-friend-message group message))
  

(define (send-friend-message #:friend friend
                             #:message message)
  (send-group-or-friend-message friend message))


(define (send-group-or-friend-message subject message)
  (define target (send subject get-id))
  (define message-chain (as-message-chain message))

  (define content (make-hash `((target . ,target)
                               (messageChain . ,(encode-message-chain message-chain)))))

  (define quote-m (send message-chain get quote%))
  (when quote-m
    (hash-set! content 'quote (send quote-m get-id)))
  
  (define command (if (equal? (object-name subject) 'object:group%)
                      "sendGroupMessage"
                      "sendFriendMessage"))

  (define bot (send subject get-bot))
  (define conn (send bot get-client-connection))
  (client-send-command! conn
                        command
                        #:content content
                        #:log? (send bot verbose?)))
      