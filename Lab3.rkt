(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex smth) (begin
                       (let ((x smth))
                         (write 'smth)
                         (display " => ")
                         (write x)
                         (display "\n")
                         x)))))
(define (zip . xss)
  (if (or (null? xss)
          (null? (trace-ex (car xss))))
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))

(define-syntax test
  (syntax-rules ()
    ((test tes ans) '(tes ans))))

(define (run-test testik)
  (write (car testik))
  (define c (eval (car testik) (interaction-environment)))
  (if (equal? c (cadr testik))
      (begin
        (display " ok\n")
        #t)
      (begin
        (display " FAIL\n")
        (display "  Expected: ")
        (write (cadr testik))
        (display " \n")
        (display "  Returned: ")
        (write c)
        (display "\n")
        #f)))

(define (run-tests sp)
  (if (null? (cdr sp))
      (run-test (car sp))
      ((lambda (x y)
         (and x y))
       (run-test (car sp))
       (run-tests (cdr sp)))))

(define (insert sp index elem)
  (if (= 0 index)
      (cons elem (insert sp (- index 1) elem))
      (if (= 0 (length sp))
          '()
          (cons (car sp) (insert (cdr sp) (- index 1) elem)))))


(define (ref elem . xs)
  (if (= (length xs) 1)
      (let* ((ind (car xs)))
        (cond
          ((string? elem) (and (not (<= (string-length elem) ind)) (string-ref elem ind)))
          ((list? elem)  (and (not (<= (length elem) ind)) (list-ref elem ind)))
          ((vector? elem) (and (not (<= (vector-length elem) ind)) (vector-ref elem ind)))))
      (let* ((ind (car xs))
             (exch (cadr xs)))
        (define t (cond
                    ((string? elem) (list (string->list elem) "str"))
                    ((vector? elem) (list (vector->list elem) "vec"))
                    ((list? elem) (list elem "lis"))))
        (and (>= (length (car t)) ind)
             (begin
               (let ((ans (insert (car t) ind exch)))
                 (cond
                   ((equal? (cadr t) "str") (and (char? exch) (list->string ans)))
                   ((equal? (cadr t) "vec") (list->vector ans))
                   ((equal? (cadr t) "lis") ans))))))
      ))

;(ref '(1 2 3) 1 0)
;(ref #(1 2 3) 1 0)
;(ref #(1 2 3) 1 #\0)
;(ref "123" 1 #\0)
;(ref "123" 1 0)
;(ref "123" 3 #\4)
;(ref "123" 5 #\4)

(define (factorize sp)
  (let* ((x (cadr (cadr sp)))
         (y (cadr (caddr sp)))
         (li sp))
    (if (= 2 (caddr (cadr li)))
        `(* (- x y) (+ x y))
        (if (equal? '+ (car li))
            `(* (+ x y) (- ( + (* x x) (* y y)) (* x y)))
            `(* (- x y) (+ (* x x) (* y y) (* x y)))
            ))))


;(define t (list (test (factorize '(- (expt x 2) (expt y 2))) (* (- x y) (+ x y)))
;                (test (factorize '(- (expt x 3) (expt y 3))) (* (- x y) (+ (* x x) (* y y) (* x y))))
;                (test (factorize '(+ (expt x 3) (expt y 3))) (* (+ x y) (- ( + (* x x) (* y y)) (* x y))))
;                (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2))) (* (- (+ first 1) (- second 1)) (+ (+ first 1) (- second 1))))))
;
;(run-tests t)