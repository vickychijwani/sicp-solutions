;;; procedure nth-root. After a few experiments, I observed that the number of
;;; average damps required to make the approximation for the n-th root converge
;;; seems to vary as the floor of the log (to the base 2) of n.

(define (nth-root x n)
  (define (log-2 n)
    (/ (log n) (log 2)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (average-damp f)
    (lambda (x) (average x (f x))))
  (let ((first-guess 1.0))
    (fixed-point ((repeated average-damp
                            (floor (log-2 n)))
                  (lambda (y)
                    (/ x (expt y (- n 1)))))
                 first-guess)))