(define (day-of-week day month year)
  (let* ((m (if (>= month 3)
                (- month 2)
                (+ 10 month)))
         (year (if (>= month 3)
                   year
                   (- year 1))))
    (remainder (+ day (quotient (- (* 13 m) 1) 5) year (quotient year 4)
         (quotient year 400) (- (quotient year 100))) 7)))

(define (roots a b c)
  (cond
    ((< (- (* b b) (* 4 a c)) 0) (list))
    ((= (- (* b b) (* 4 a c)) 0) (list (- (/ b (* 2 a)))))
    (else (list (/ (+ (- b) (sqrt (- (* b b) (* 4 a c))))
                   (* 2 a)) (/ (- (- b) (sqrt (- (* b b) (* 4 a c)))) (* 2 a))))))

(define (my-gcd a b)
  (if (= (min (abs a) (abs b)) 0)
      (max (abs a) (abs b))
      (if (and (= 0 (remainder (min (abs a) (abs b)) 2))
               (= 0 (remainder (max (abs a) (abs b)) 2)))
          (* 2 (my-gcd (/ (max (abs a) (abs b)) 2) (/ (min (abs a) (abs b)) 2)))
          (cond
            ((= 0 (remainder (max (abs a) (abs b)) 2))
             (my-gcd (/ (max (abs a) (abs b)) 2) (min (abs a) (abs b))))
            ((= 0 (remainder (min (abs a) (abs b)) 2))
             (my-gcd (max (abs a) (abs b)) (/ (min (abs a) (abs b)) 2)))
            (else (my-gcd (- (max (abs a) (abs b)) (min (abs a) b)) (min (abs a) (abs b))))))))

(define (my-lcm a b)
  (/ (* a b) (my-gcd a b)))


(define (loop n i)
  (or (= 1 i) (and (not (= 0 (remainder n i))) (loop n (- i 1)))))
   
(define (prime? n)
  (loop n (+ 1 (round (sqrt n)))))


(my-gcd 3542 -2464) 
(my-lcm 3 4)       
(prime? 11)        
(prime? 12)       