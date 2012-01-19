;;; procedure double

(define (double f)
  (lambda (x)
    (f (f x))))


;;; We are to compute the value of:

(((double (double double)) inc) 5)

;;; =>

(((double
   (lambda (x)
     (double (double x))))
  inc) 5)

;;; Let g =

(lambda (x)
  (double (double x)))

;;; Therefore, =>

(((double g) inc) 5)

;;; =>

(((lambda (y)
    (g (g y)))
  inc) 5)

;;; Applying g to inc once returns a procedure which increments its
;;; argument by 4 (call it inc4) => Applying g to inc twice quadruples
;;; the effect of the above, so we get inc16. Ans: (inc16 5) = 21
