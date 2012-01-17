;;; iterative procedure for k-term finite continued fraction

(define (cont-frac n d k)
  (define (iterate result i)
    (if (= i 0)
        result
        (iterate (/ (n i) (+ (d i) result)) (- i 1))))
  (iterate 0 k))

;;; test for above procedure; should evaluate to golden ratio (/ 1.0 \phi) =
;;; 0.6180 (to 4 decimal places)

(let ((k 12))
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

;;; k = 12 gets us the required accuracy