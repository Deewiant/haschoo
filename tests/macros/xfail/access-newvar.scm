(define-syntax m (syntax-rules () ((m) x)))
(let ((x 'bad))
  (write (m))
  (newline))
