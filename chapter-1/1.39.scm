;;; iterative procedure for k-term finite continued fraction

(define (cont-frac n d k)
  (define (iterate result i)
    (if (= i 0)
        result
        (iterate (/ (n i) (+ (d i) result)) (- i 1))))
  (iterate 0 k))

;;; Lambert's formula: continued fraction representation of the tangent function
;;; (n i) = x, (- (* x x)), (- (* x x)), (- (* x x)), (- (* x x)), ...
;;; (d i) = (- (* 2 i) 1)

(define (tan-cf x k)
  (cont-frac
   (lambda (i)
     (if (= i 1)
         x
         (- (* x x))))
   (lambda (i)
     (- (* 2 i) 1))
   k))