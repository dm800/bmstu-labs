(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d)))) ; O((b - a) / d)

(define (my-flatten xs)
  (if (list? xs)
      (if (= 1 (length xs))
          (append '() (my-flatten (car xs)))
          (append '() (my-flatten (car xs)) (my-flatten (cdr xs))))
      (list xs))) ; O(len(xs)^2)

(define (my-element? x xs)
  (and (not (null? xs)) (or (equal? (car xs) x) (my-element? x (cdr xs))))) ;O(len(xs))

(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs))))) ; O(len(xs))

(define (my-fold-right op xs)
  (if (= 1 (length xs))
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs))))) ; O(len(xs))

(define (my-fold-left op xs)
  (if (= 1 (length xs))
      (car xs)
      (let* ((rev (reverse xs)))
        (op (my-fold-left op (reverse (cdr rev))) (car rev))))) ; O(len(xs)^2)

;;;;;;;;;;

(define (my-element? x xs)
  (and (not (null? xs)) (or (equal? (car xs) x) (my-element? x (cdr xs))))) ;O(len(xs))

(define (count x xs)
  (if (null? xs)
      0
      (if (equal? x (car xs))
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs))))) ; O(len(xs))

(define (set? sp)
  (or (null? sp) (and (= (count (car sp) (cdr sp)) 0) (set? (cdr sp))))) ; O(len(sp)^2)

(define (list->set sp)
  (if (set? sp)
      sp
      (if (= (count (car sp) (cdr sp)) 0)
          (cons (car sp) (list->set (cdr sp)))
          (list->set (cdr sp))))) ; O(len(sp)^2)

(define (union mn1 mn2)
  (list->set (append mn1 mn2))) ; O(len(mn1 + mn2)^3);

(define (intersection mn1 mn2)
  (if (= 0 (length mn1))
      '()
      (if (my-element? (car mn1) mn2)
          (cons (car mn1) (intersection (cdr mn1) mn2))
          (intersection (cdr mn1) mn2)))) ; O(len(mn1)*len(mn2))

(define (difference mn1 mn2)
  (if (null? mn1)
      '()
      (if (my-element? (car mn1) mn2)
          (difference (cdr mn1) mn2)
          (cons (car mn1) (difference (cdr mn1) mn2))))) ; O(len(mn1)*len(mn2))

(define (symmetric-difference mn1 mn2)
  (difference (union mn1 mn2) (intersection mn1 mn2))) ; O(len(mn1 + mn2)^3

(define (set-eq? mn1 mn2)
  (and (null? (difference mn1 mn2)) (null? (difference mn2 mn1)))) ; O(len(mn1)*len(mn2))

;;;;;;;;;;;;;;;;;;;;;;

(define (string-trim-left st)
  (let* ((sp (string->list st)))
    (if (or (equal? #\newline (car sp))
            (equal? #\tab (car sp))
            (equal? #\space (car sp)))
        (string-trim-left (list->string (cdr sp)))
        st))) ; O(len(st)

(define (string-trim-right st)
  (let* ((sp (reverse (string->list st))))
    (if (or (equal? #\newline (car sp))
            (equal? #\tab (car sp))
            (equal? #\space (car sp)))
        (string-trim-right (list->string (reverse (cdr sp))))
        st))) ; O(len(st)^2)

(define (string-trim st)
  (string-trim-right (string-trim-left st))) ; O(len(st)^3)

(define (string-prefix? pr st)
  (or (equal? pr st)
      (and (not (equal? "" st))
           (or (equal? "" pr)
               (and (equal? (car (string->list pr)) (car (string->list st)))
                    (string-prefix? (list->string (cdr (string->list pr)))
                                    (list->string (cdr (string->list st))))))))) ; O(len(st))

(define (string-suffix? pr st)
  (string-prefix? (list->string (reverse (string->list pr)))
                  (list->string (reverse (string->list st))))) ; O(len(st))

(define (string-infix? pr st)
  (if (>= (length (string->list pr)) (length (string->list st)))
      (string-prefix? pr st)
      (or (string-prefix? pr st) (string-infix? pr (list->string
                                                    (cdr (string->list st))))))) ; O(len(st)^2)

(define (find-one st spl)
  (if (string-prefix? spl st)
      ""
      (if (string-infix? spl st)
          (string-append (string (car (string->list st)))
                         (find-one (list->string (cdr (string->list st))) spl))
          ""))) 

(define (sliced st pr)
  (if (equal? "" st)
      pr
      (sliced (list->string (cdr (string->list st))) (list->string (cdr (string->list pr))))))

(define (skip-one st spl)
  (if (string-infix? spl st)
      (if (string-prefix? spl st)
          (sliced spl st)
          (skip-one (list->string (cdr (string->list st))) spl))
      ""))

(define (string-split st split)
  (if (string-infix? split st)
      (cons (find-one st split) (string-split (skip-one st split) split))
      (list st)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (multi sp)
  (if (null? sp)
      1
      (* (car sp) (multi (cdr sp)))))

(define (make-multi-vector sp . xs)
  (if (null? xs)
      (list 'multi-vector sp (make-vector (multi sp)))
      (list 'multi-vector sp (make-vector (multi sp) (car xs)))))

(define (return-index razm ind)
  (if (= 1 (length ind))
      (car ind)
      (+ (* (car ind) (multi (cdr razm))) (return-index (cdr razm) (cdr ind)))))

(define (multi-vector? vect)
  (and (list? vect) (not (null? vect)) (equal? (car vect) 'multi-vector)))

(define (multi-vector-set! vect coord znach)
  (vector-set! (caddr vect) (return-index (cadr vect) coord) znach))

(define (multi-vector-ref vect coord)
  (vector-ref (caddr vect) (return-index (cadr vect) coord)))

;;;;;;;;;;;;;;;;;;;;;;

(define (o . xs)
  (define (f x xs)
    (if (null? xs)
        x
        ((car xs) (f x (cdr xs)))))
  (lambda x (f (car x) xs)))

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

((o f g h) 1)
((o f g) 1)
((o h) 1) 
((o) 1)