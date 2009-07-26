(define-syntax m (syntax-rules () ((m x) x)))
(let ((x 'ok))
  (write (m x))
  (newline))
