(define (interpret prog stack)
  (let* ((stack stack)
         (prog prog)
         (cur 0)
         (ret '())
         (sl '()))
    (main prog cur stack ret sl 0)))

(define (main prog cur stack ret sl ign)
  (if (>= cur (vector-length prog))
      stack
      (let ((word (vector-ref prog cur)))
        (if (= 0 ign)
            (cond
              ((equal? '+ word) (main prog (+ 1 cur) (cons (+ (car stack) (cadr stack)) (cddr stack)) ret sl 0))
              ((equal? '- word) (main prog (+ 1 cur) (cons (- (cadr stack) (car stack)) (cddr stack)) ret sl 0))
              ((equal? '* word) (main prog (+ 1 cur) (cons (* (car stack) (cadr stack)) (cddr stack)) ret sl 0))
              ((equal? '/ word) (main prog (+ 1 cur) (cons (/ (cadr stack) (car stack)) (cddr stack)) ret sl 0))
              ((equal? 'mod word) (main prog (+ 1 cur) (cons (remainder (cadr stack) (car stack)) (cddr stack)) ret sl 0))
              ((equal? 'neg word) (main prog (+ 1 cur) (cons (- (car stack)) (cdr stack)) ret sl 0))
              ((equal? '= word) (main prog (+ 1 cur) (cons (if (= (car stack) (cadr stack))
                                                               -1
                                                               0) (cddr stack)) ret sl 0))
              ((equal? '> word) (main prog (+ 1 cur) (cons (if (< (car stack) (cadr stack))
                                                               -1
                                                               0) (cddr stack)) ret sl 0))
              ((equal? '< word) (main prog (+ 1 cur) (cons (if (> (car stack) (cadr stack))
                                                               -1`
                                                               0) (cddr stack)) ret sl 0))
              ((equal? 'not word) (main prog (+ 1 cur) (cons (if (= 0 (car stack))
                                                                 -1
                                                                 0) (cdr stack)) ret sl 0))
              ((equal? 'and word) (main prog (+ 1 cur) (cons (if (= 0 (* (car stack) (cadr stack)))
                                                                 0
                                                                 -1) (cddr stack)) ret sl 0))
              ((equal? 'or word) (main prog (+ 1 cur) (cons (if (or (not (= 0 (car stack))) (not (= 0 (cadr stack))))
                                                                -1
                                                                0) (cddr stack)) ret sl 0))
              ((equal? 'drop word) (main prog (+ 1 cur) (cdr stack) ret sl 0))
              ((equal? 'swap word) (main prog (+ 1 cur) (cons (cadr stack) (cons (car stack) (cddr stack))) ret sl 0))
              ((equal? 'dup word) (main prog (+ 1 cur) (cons (car stack) stack) ret sl 0))
              ((equal? 'over word) (main prog (+ 1 cur) (cons (cadr stack) stack) ret sl 0))
              ((equal? 'rot word) (main prog (+ 1 cur) (cons (caddr stack) (cons (cadr stack) (cons (car stack) (cdddr stack)))) ret sl 0))
              ((equal? 'depth word) (main prog (+ 1 cur) (cons (length stack) stack) ret sl 0))
              ((equal? 'define word) (main prog (+ 1 cur) stack ret (cons (list (vector-ref prog (+ 1 cur)) (+ 2 cur)) sl) 2))
              ((equal? 'if word) (main prog (+ 1 cur) (cdr stack) ret sl (if (= 0 (car stack))
                                                                       1
                                                                       0)))
              ((equal? 'end word) (main prog (car ret) stack (cdr ret) sl 0))
              ((equal? 'endif word) (main prog (+ 1 cur) stack ret sl 0))
              ((equal? 'exit word) (main prog (car ret) stack (cdr ret) sl 0))
              ((number? word) (main prog (+ 1 cur) (cons word stack) ret sl 0))
              (else (main prog (cadr (assoc word sl)) stack (cons (+ 1 cur) ret) sl 0)))
                     
            (cond
              ((and (equal? 'endif word) (= ign 1)) (main prog (+ 1 cur) stack ret sl 0))
              ((and (equal? 'end word) (= ign 2)) (main prog (+ 1 cur) stack ret sl 0))
              (else (main prog (+ 1 cur) stack ret sl ign))))
          )))