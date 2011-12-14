;;; recursive higher order procedure for accumulation

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;;; sum in terms of accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b))

;;; product in terms of accumulate
(define (product term a next b)
  (accumulate * 1 term a next b))