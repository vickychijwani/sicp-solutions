(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (let ((tolerance 0.00001))
      (< (abs (- x y)) tolerance)))
  (define (try guess iterations)
    (let ((next (f guess)))
      (display "trying ")
      (display guess)
      (newline)
      (if (close-enough? guess next)
          ((lambda ()
             (display iterations)
             (display " iterations")
             next))
          (try next (+ iterations 1)))))
  (try first-guess 1))

;;; finding a solution to x^x = 1000 as a fixed point of the transformation
;;; (lambda (x) (/ (log 1000.0) (log x))). The first guess cannot be 1.0 because that
;;; would lead to division by (log 1) = 0.

(define first-guess 2.0)

;;; without average damping: 34 iterations when first-guess = 2.0

(fixed-point (lambda (x) (/ (log 1000.0) (log x))) first-guess)

;;; with average damping: 9 iterations when first-guess = 2.0

(define (average x y)
  (/ (+ x y) 2))
(fixed-point (lambda (x) (average x (/ (log 1000.0) (log x)))) first-guess)

;;; average damping gives us an improvement of 34/9 = 3.8 times, starting from
;;; an initial guess of 2.0