(define (derivative expr)
  (cond
    ((not (list? expr)) (if (number? expr)
                            0
                            1))
    ((and (equal? '+ (car expr)) (= 2 (length expr))) (derivative (cadr expr)))
    ((equal? '+ (car expr)) `(+ ,(derivative (cadr expr)) ,(derivative (cons '+ (cddr expr)))))
    ((and (equal? '* (car expr)) (= (length expr) 3))
     `(+ (* ,(derivative (cadr expr)) ,(caddr expr)) (* ,(derivative (caddr expr)) ,(cadr expr))))
    ((equal? '* (car expr))
     `(+ (* ,(derivative (cadr expr)) ,(cons '* (cddr expr))) (* ,(cadr expr)
                                                                 ,(derivative (cons '* (cddr expr))))))
    ((and (equal? 'expt (car expr)) (not (number? (cadr expr))))
     `(* ,(caddr expr) (expt ,(cadr expr) ,(- (caddr expr) 1)) ,(derivative (cadr expr))))
    ((equal? 'expt (car expr)) `(* (log ,(cadr expr)) ,expr ,(derivative (caddr expr))))
    ((equal? 'exp (car expr)) `(* ,expr ,(derivative (cadr expr))))
    ((and (equal? '- (car expr)) (= 2 (length expr))) `(* -1 ,(derivative (cadr expr))))
    ((equal? '- (car expr)) `(- ,(derivative (cadr expr)) ,(derivative (caddr expr))))
    ((equal? '/ (car expr))
     `(/ (- (* ,(derivative (cadr expr)) ,(caddr expr)) (* ,(cadr expr) ,(derivative (caddr expr))))
         (expt ,(caddr expr) 2)))
    ((equal? 'cos (car expr)) `(* ,(derivative (cadr expr)) (- (sin ,(cadr expr)))))
    ((equal? 'sin (car expr)) `(* ,(derivative (cadr expr)) (cos ,(cadr expr))))
    ((equal? 'log (car expr)) `(* (/ ,(cadr expr)) ,(derivative (cadr expr))))
    (else expr)))

(derivative (quote (expt 5 x)))