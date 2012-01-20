;;; procedure smooth

(define (smooth f)
  (define (average a b c)
    (/ (+ a b c) 3.0))
  (let ((dx 0.00001))
    (lambda (x)
      (average (f (- x dx)) (f x) (f (+ x dx))))))

;;; n-fold (repeated) smooth

(define (repeated-smooth f n)
  ((repeated smooth n) f))