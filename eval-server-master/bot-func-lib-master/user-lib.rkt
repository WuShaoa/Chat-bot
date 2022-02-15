#lang racket

(provide init-bot-func-lib)


(define (init-bot-func-lib env)
  (define lib-dir "E:/Program Files/Mirai/chat-bot-master/eval-server-master/bot-func-lib-master")
  (define (eval-file path env)
    (define in (open-input-file path #:mode 'text))
    (let loop ()
      (define expr (read in))
      (when (not (eof-object? expr))
        (eval expr env)
        (loop))))

  (displayln "lib loading")
  (for ((filename
         '("base"
           "media-output"
           "menu"
           "ascii-screen"

           "games/bulls-and-cows"
           "games/simple-guess-number"
           "games/tic-tac-toe"
           "games/tic-tac-toe-online"
           "games/cards/card"
           "games/cards/guess-card"

           "fz-rkt-lib"

           "tools/analog-clock"
           "tools/digital-clock"
           "tools/blink-text-gif"
           "tools/dict"
           "tools/joke"
           "tools/citys"
           "tools/query-weather"
           "tools/pic"
           "tools/sorry"
           "tools/qinghua"
           "tools/osu")))
    (displayln (string-append " " filename))
    (eval-file (format "~A/~A.rkt" lib-dir filename) env)))