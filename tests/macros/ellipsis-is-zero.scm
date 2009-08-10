(define-syntax m (syntax-rules () ((m (x ...)) 'ok)))
(write (m ()))(newline)
(define-syntax m (syntax-rules () ((m (x y ...)) 'x)))
(write (m (ok)))(newline)
