;;; iterative higher order procedure for product
;;; for implementations of the factorial and Wallis' formula, refer to 1.31-a.scm

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))