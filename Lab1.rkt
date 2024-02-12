(define (my-odd x)
  (= (remainder x 2) 1))

(define (my-even x)
  (= (remainder x 2) 0))

(define (pow x y)
  (if (= y 1)
      x
      (* x (pow x (- y 1)))))