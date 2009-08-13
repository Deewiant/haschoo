(define-syntax m (syntax-rules () ((m (x ...) ...) '(#((x ...) (x ...)) ...)))) (m (a) (b))
(write (if (equal? (m (a) (b)) '(#((a) (a)) #((b) (b)))) 'ok 'bad))
(newline)
