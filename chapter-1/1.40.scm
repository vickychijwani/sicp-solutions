;;; Newton's method

(define (deriv f)
  (lambda (x)
    (let ((dx 0.00001))
        (/ (- (f (+ x dx)) (f x))
        dx))))

(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))

(define (newtons-method f guess)
  (define (newton-transform f)
    (lambda (x)
      (- x (/ (f x) ((deriv f) x)))))
  (fixed-point-of-transform f newton-transform guess))

;;; procedure (cubic)

(define (cubic a b c)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;;; f(x) = x^3 - 5*x^2 + 6*x. Roots are 0, 2, 3. Only one of the roots will be
;;; returned by (newtons-method) below, depending on the value of guess.

(let ((guess 1.0))
    (newtons-method (cubic -5 6 0) guess))