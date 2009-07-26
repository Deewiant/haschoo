(define-syntax m (syntax-rules (x) ((m x) 'ok)))
(write (m x))
(newline)
