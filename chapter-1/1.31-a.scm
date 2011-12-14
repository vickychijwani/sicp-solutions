;;; recursive higher order procedure for product

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product identity 1 inc n))

;;; Wallis' formula:
;;; pi/4 =~ wallis(n) = (2.4.4.6.6.8 ... [m terms]) / (3.3.5.5.7.7 ... [m terms])
;;;                   = 2 * (4^2 * 6^2 * 8^2 * ... * (2n+2) [n terms]) /
;;;                         (3^2 * 5^2 * 7^2 * ... * (2n+1)^2 [n terms])
;;;
;;; NOTE: the last term of the series in the numerator MUST NOT BE SQUARED, thus:
;;;
;;; (eq1): (2n+2)*wallis(n) = 2 * (4^2 * 6^2 * 8^2 * ... * (2n+2)^2 [n terms]) /
;;;                               (3^2 * 5^2 * 7^2 * ... * (2n+1)^2 [n terms])
;;;
;;; However, this is an inefficient way to compute wallis(n), as the products
;;; in the numerator and denominator will quickly overflow the floating-point
;;; limit of the machine. (eq1) could be re-written as:
;;;
;;; (2n+2)*wallis(n) = 2*((4/3)^2 * (6/5)^2 * (8/7)^2 * ... * ((2n+2)/(2n+1))^2)
;;;
;;; Now wallis(n) <= wallis(1) for all positive n. This approach is used below:

(define (wallis n)
  (define (square x) (* x x))
  (define (2-k-plus-x k x) (+ (* 2.0 k) x))
  (define (wallis-term k)
    (square (/ k (- k 1))))
  (define (wallis-next k) (+ k 2))
  (/ (* 2.0
        (product wallis-term 4 wallis-next (2-k-plus-x n 2)))
     (2-k-plus-x n 2)))