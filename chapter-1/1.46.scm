;;; procedure iterative-improve

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        ((lambda () (display guess) (newline) guess))
        (iter (improve guess))))
  iter)

;;; procedure sqrt

(define (sqrt n)
  (define (square x) (* x x))
  (define (average x y) (/ (+ x y) 2.0))
  (let ((tolerance 0.0001)
        (first-guess 1.0))
    ((iterative-improve (lambda (guess)
                          (< (abs (- (square guess) n)) tolerance))
                        (lambda (guess)
                          (average guess (/ n guess))))
     first-guess)))

;;; procedure fixed-point

(define (fixed-point f first-guess)
  (let ((tolerance 0.00001))
    ((iterative-improve (lambda (guess)
                          (< (abs (- guess (f guess))) tolerance))
                        f)
     first-guess)))