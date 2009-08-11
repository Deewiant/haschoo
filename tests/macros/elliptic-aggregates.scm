(define-syntax m (syntax-rules () ((m (x #(a b) ...)) '((a b) ...))))
(write (if (null? (m (x))) 'ok 'bad))(newline)
           

(define-syntax m (syntax-rules () ((m (x ...) y) '((x y)...))))
(write (if (equal? (m (a b) x) '((a x) (b x))) 'ok 'bad))(newline)
