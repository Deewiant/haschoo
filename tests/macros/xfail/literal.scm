(define-syntax m (syntax-rules (x) ((m x) 'ok)))
(let ((x 0))
  (write (m x))
  (newline))
