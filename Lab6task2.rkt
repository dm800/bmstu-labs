;; Конструктор потока
(define (make-stream items . eos)
  (if (null? eos)
      (make-stream items #f)
      (list items (car eos))))

;; Запрос текущего символа
(define (peek stream)
  (if (null? (car stream))
      (cadr stream)
      (caar stream)))

;; Запрос первых двух символов
(define (peek2 stream)
  (if (null? (car stream))
      (cadr stream)
      (if (null? (cdar stream))
          (list (caar stream))
          (list (caar stream) (cadar stream)))))

;; Продвижение вперёд
(define (next stream)
  (let ((n (peek stream)))
    (if (not (null? (car stream)))
        (set-car! stream (cdr (car stream))))
    n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;<Program>  ::= <Articles> <Body> .
;<Articles> ::= <Article> <Articles> | .
;<Article>  ::= define word <Body> end .
;<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .
(define (word? symb)
  (and symb (not (or (equal? symb 'define) (equal? symb 'if) (equal? symb 'endif) (equal? symb 'end)))))

(define (parse vector)
  (define stream (make-stream (vector->list vector)))
  (call-with-current-continuation
   (lambda (error)
     (program stream error))))

(define (program stream error)
  (list (articles stream error) (body stream error)))

(define (articles stream error)
  (let ((art (article stream error)))
    (if art
        (cons art (articles stream error))
        '())))

(define (article stream error)
  (and (equal? (peek stream) 'define)
       (begin
         (next stream)
         (and (not (word? (peek stream))) (error #f)) 
         (let ((bod (list (next stream) (body stream error))))
           (if (equal? (next stream) 'end)
               bod
               (error #f))))))

(define (body stream error)
  (let ((symb (peek stream)))
    (cond
      ((number? symb) (begin
                        (next stream)
                        (cons symb (body stream error))))
      ((word? symb) (begin
                      (next stream)
                      (cons symb (body stream error))))
      ((equal? symb 'if) (begin
                           (next stream)
                           (let ((bod (list 'if (body stream error))))
                             (if (equal? 'endif (peek stream))
                                 (begin
                                   (next stream)
                                   (cons bod (body stream error)))
                                 (error #f)))))
      (else '()))))

(parse #(if 1 if 1 if 1 endif endif endif))
(parse #(if 1 if 1 if 1 endif endif))
(parse #(define + end define + end))
(parse #(define + end define +))

(parse #(define + define + end end))
(parse #(define end end))
(parse #(define if end))
(parse #(define if  end endif))
(parse #())
(parse #(+ + +))