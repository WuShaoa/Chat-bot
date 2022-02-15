#lang racket

(provide bot-id
         bot-nickname
         admin-ids
         mirai-ws-server-config)


(define bot-id 2127006713)
(define bot-nickname "goodnight")
(define admin-ids '(1131309200))

(define mirai-ws-server-config
  (hash 'hostname "localhost"
        'port 8899))