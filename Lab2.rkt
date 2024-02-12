(define (count x xs)
  (if (null? xs)
      0
      (if (equal? x (car xs))
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs)))))

(define (delete pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (append (list (car xs)) (delete pred? (cdr xs))))))

(define (iterate f x n)
  (if (= n 1)
      (list x)
      (append (iterate f x (- n 1)) (list (f (car (reverse (iterate f x (- n 1)))))))))

(define (intersperse e xs)
  (if (<= (length xs) 1)
      xs
      (append (list (car xs) e) (intersperse e (cdr xs)))))

(define (any? pred? xs)
  (and (not (null? xs)) (or (any? pred? (cdr xs)) (pred? (car xs)))))

(define (all? pred? xs)
  (or (null? xs) (and (all? pred? (cdr xs)) (pred? (car xs)))))

(define (o . xs)
  (define (f x xs)
    (if (null? xs)
        x
        ((car xs) (f x (cdr xs)))))
  (lambda x (f (car x) xs)))
