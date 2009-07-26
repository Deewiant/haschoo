(letrec-syntax ((m (syntax-rules () ((m x) x) ((m x y ...) (m y ...)))))
  (write (m 'bad 'ok))
  (newline))
