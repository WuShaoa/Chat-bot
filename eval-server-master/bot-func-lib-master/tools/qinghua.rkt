(require net/url)
(require json)
(require srfi/13)

(define (get-prattle)

  (define (create-url)
    (string->url "https://api.ghser.com/qinghua/"))

  (define-values [status headers in]
    (http-sendrecv/url (create-url)))

  (define result (port->string in))

  (display result)
)

(define-name-command 情话 (get-prattle))