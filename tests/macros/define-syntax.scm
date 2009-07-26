(define-syntax m1 (syntax-rules () ((m1 x) (m2 x))))
(define-syntax m2 (syntax-rules () ((m2 x) x)))
(write (m1 'ok))
(newline)
