(define-syntax m (syntax-rules ()
                    ((m (x ...) (y ...))(begin(write 'BAD)(newline)'((x y) ...)))))
(m (a b) (x))
