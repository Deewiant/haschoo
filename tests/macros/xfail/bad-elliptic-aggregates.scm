(define-syntax m (syntax-rules ()
                    ((m (x ...) (y ...))(begin(write 'BAD)(newline)))))
(m (a b) (x))
