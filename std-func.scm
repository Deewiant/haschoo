; (R5RS 6.3.2)
(define list-tail
  (lambda (x k)
    (if (zero? k)
        x
        (list-tail (cdr x) (- k 1)))))

; (R5RS 6.4)
(define force
  (lambda (object)
      (object)))
