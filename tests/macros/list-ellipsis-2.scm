(define-syntax m
  (syntax-rules ()
    ((m () b) b)
    ((m ((_1 _2) (x y) ...) b) (m ((x y) ...) b))))
(write (m ((1 2) (3 4)) 'ok))
(newline)
