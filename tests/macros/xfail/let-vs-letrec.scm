(let-syntax ((m (syntax-rules () ((m x) x) ((m x y ...) (m y ...)))))
  (write (m 'BAD 'BAD))
  (newline))
