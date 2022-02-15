#lang racket

(provide init-user-lib)
(require "../bot-func-lib-master/user-lib.rkt")

(define (init-user-lib env)
  (init-bot-func-lib env))