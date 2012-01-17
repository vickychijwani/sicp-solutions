;;; recursive procedure for k-term finite continued fraction

(define (cont-frac n d k)
  (define (recurse i)
    (if (= i k)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (recurse (+ i 1))))))
  (recurse 1))

;;; test for above procedure; should evaluate to golden ratio (/ 1.0 \phi) =
;;; 0.6180 (to 4 decimal places)

(let ((k 12))
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

;;; k = 12 gets us the required accuracy