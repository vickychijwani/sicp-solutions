;;; enhanced (make-rat) procedure that handles negative arguments correctly

;;; load file containing GCD procedure
(load "../chapter-1/1.33.scm")

(define (make-rat n d)
  (let ((n-g-abs (abs (/ n (gcd n d))))
        (d-g-abs (abs (/ d (gcd n d)))))
    (if (or (and (> n 0) (> d 0)) (and (< n 0) (< d 0)))
        (cons n-g-abs d-g-abs)
        (cons (- n-g-abs) d-g-abs))))