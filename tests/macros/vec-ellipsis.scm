(define-syntax m (syntax-rules ()
  ((m #(x y) ...) (append (list x ...) (list y ...)))))

(letrec ((x (m #(1 2) #(3 4))))
  (if (equal? x '(1 3 2 4))
      (begin (write 'ok)(newline))))
