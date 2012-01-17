;;; iterative procedure for k-term finite continued fraction

(define (cont-frac n d k)
  (define (iterate result i)
    (if (= i 0)
        result
        (iterate (/ (n i) (+ (d i) result)) (- i 1))))
  (iterate 0 k))

;;; continued fraction for e-2

(+ 2
 (let ((k 10))
   (cont-frac (lambda (i) 1.0)
              (lambda (i)
                (if (= (remainder (+ i 1) 3) 0)
                    (/ (+ (* 2 i) 2) 3)
                    1.0))
              k)))

;;; k = 10 gets us e accurate to 6 decimal places