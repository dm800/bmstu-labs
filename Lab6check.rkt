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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-frac str)
  (define stream
    (make-stream (string->list str) (integer->char 0)))
  
  (call-with-current-continuation
   (lambda (error)
     (ans stream error)
     (equal? (peek stream) (integer->char 0)))))

(define (char-sign? char)
  (or (equal? char #\+) (equal? char #\-)))

; <ans> ::= <sign> <rest> |
;           <rest>
(define (ans stream error)
  (let ((current (peek stream)))
    (and (char-sign? current) (next stream))
    (rest stream error)))


; <rest> ::= <number> '/' <number>
(define (rest stream error)
  (number stream error)
  (and (not (equal? (peek stream) #\/)) (error #f))
  (next stream)
  (number stream error))
  

; <number> ::= DIGIT <tail>
(define (number stream error)
  (if (char-numeric? (peek stream))
      (begin
        (next stream)
        (tail stream error))
      (error #f)))

; <tail> ::= <empty> | DIGIT <tail>
(define (tail stream error)
  (if (char-numeric? (peek stream))
      (begin
        (next stream)
        (tail stream error))
      (and (not (or (equal? (peek stream) #\/) (equal? (peek stream) (integer->char 0)))) (error #f))))

(check-frac "-2/1")