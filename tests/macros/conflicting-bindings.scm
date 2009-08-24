(define-syntax m (syntax-rules () ((m n1 n2) (let ((x n1)) n2))))
(define (f x) (m x x))
(write (if (= (f 2) 2) 'ok 'bad))(newline)
