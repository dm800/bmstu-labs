(define (memoized-factorial n)
  (let* ((known-results '()))
    (if (<= n 1)
        1
        (let* ((res (assoc n known-results)))
          (if res
              res
              (begin
                (set! res (* (memoized-factorial (- n 1)) n))
                (set! known-results (cons (list n res) known-results))
                res))))))

;;;;;;;;;;;;;;;;;;;;

(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b) (list a (delay b)))))

(define (lazy-car sp)
  (car sp))

(define (lazy-cdr sp)
  (force (cadr sp)))

(define (lazy-head xs k)
  (if (= k 1)
      (list (car xs))
      (cons (car xs) (lazy-head (lazy-cdr xs) (- k 1)))))

(define (lazy-ref xs k)
  (if (= k 0)
      (car xs)
      (lazy-ref (lazy-cdr xs) (- k 1))))

(define (naturals st)
  (lazy-cons st (naturals (+ st 1))))

(define (lazy-fact n p)
  (lazy-cons n (lazy-fact (* n p) (+ 1 p))))

(define (lazy-factorial n)
  (lazy-ref (lazy-fact 1 1) n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (work current)
  (let* ((char (read-char)))
    (if (eof-object? char)
        '()
        (if (or (equal? #\space char) (equal? #\newline char) (equal? #\tab char))
            (if (not (equal? current ""))
                (cons current (work ""))
                (work ""))
            (work (list->string (append (string->list current) (list char))))))))
        

(define (read-words)
  (work ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax define-struct
  (syntax-rules ()
    ((define-struct name (a ...))
     (begin
       (begin
         (eval `(define (,(string->symbol (string-append (symbol->string 'name) "?")) elem)
                  (and (pair? elem) (equal? (car elem) 'name))) (interaction-environment))
         (eval `(define (,(string->symbol (string-append "make-" (symbol->string 'name))) a ...)
                  `(name ,a ...)) (interaction-environment))
         (eval `(define (,(string->symbol (string-append (symbol->string 'name)
                                                         "-" (symbol->string 'a))) elem)
                  (define t '(name a ...))
                  (vector-ref (list->vector elem) (- (length t) (length (member 'a t)))))
               (interaction-environment)) ...)
       (eval `(define-syntax ,(string->symbol (string-append "set-" (symbol->string 'name)
                                                             "-" (symbol->string 'a) "!"))
                (syntax-rules ()
                  ((,(string->symbol (string-append "set-" (symbol->string 'name)
                                                    "-" (symbol->string 'a) "!")) obj elem)
                   (begin
                     (define t #(name a ...))
                     (define k (list->vector obj))
                     (vector-set! k (- (length (vector->list t))
                                       (length (member 'a (vector->list t)))) elem)
                     (set! obj (vector->list k))))))
                 (interaction-environment)) ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-data
  (syntax-rules ()
    ((define-data main ((a var1 ...) ...))
     (begin
       (eval `(define (,(string->symbol (string-append (symbol->string 'main) "?")) elem)
                (and (pair? elem) (or (equal? (car elem) (car '(a var1 ...))) ...)))
             (interaction-environment))
       (eval `(define (a var1 ...)
                `(a ,var1 ...))
             (interaction-environment)) ...))))

(define (help sp1 sp2)
  (if (= (length sp1) 0)
      '()
      (cons `(,(car sp2) ,(car sp1)) (help (cdr sp1) (cdr sp2)))))


(define-syntax match
  (syntax-rules ()
    ((match f ((pattern1 var1 ...) expr1)
               ((pattern2 var2 ...) expr2)
               ...)
     (begin
       (cond
         ((and (equal? (car f) 'pattern1) (= (length f) (length '(pattern1 var1 ...))))
          (let* ((k (help (cdr f) '(var1 ...))))
            (let* ((var1 (cadr (assoc 'var1 k))) ...)
              expr1)))
         ((and (equal? (car f) 'pattern2) (= (length f) (length '(pattern2 var2 ...))))
          (let* ((k (help (cdr f) '(var2 ...))))
            (let* ((var2 (cadr (assoc 'var2 k))) ...)
              expr2)))
         ...
         )))))