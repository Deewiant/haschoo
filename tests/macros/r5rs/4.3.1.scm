(let-syntax ((when (syntax-rules ()
                     ((when test stmt1 stmt2 ...)
                      (if test
                          (begin stmt1
                                 stmt2 ...) #f)))))
  (let ((if #t))
    (when if (set! if 'ok))
    (write if)))
(newline)

(let ((x 'ok))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'bad))
      (write (m)))))
(newline)
