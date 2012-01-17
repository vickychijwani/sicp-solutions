;;; computing the golden ratio \phi from the function (lambda (x) (+ 1 (/ 1 x)))

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (let ((tolerance 0.00001))
        (< (abs (- x y)) tolerance)))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;; evaluate the following s-exp; you get \phi ~= 1.61803

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)