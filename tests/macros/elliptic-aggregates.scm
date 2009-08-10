(define-syntax m (syntax-rules () ((m (x #(a b) ...)) '((a b) ...))))
(if (null? (m (x)))
    (begin (write 'ok)(newline)))

(define-syntax m (syntax-rules () ((m (x ...) (y)) '((x y)...))))
(if (equal? (m (a b) x) '((a x) (b x)))
    (begin (write 'ok)(newline)))
