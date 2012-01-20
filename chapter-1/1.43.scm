;;; procedure repeated

(define (repeated f n)
  (cond ((> n 1)
         (compose f (repeated f (- n 1))))
        (else f)))