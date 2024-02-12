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

(define (list->integer sp)
  (if (null? sp)
      0
      (+ (* (expt 10 (- (length sp) 1)) (- (char->integer (car sp)) 48)) (list->integer (cdr sp)))))

(define (skip? char)
  (or
   (equal? char #\newline)
   (equal? char #\tab)
   (equal? char #\space)))

(define (scan-many-fracs str)
  (define stream
    (make-stream (string->list str) (integer->char 0)))
  
  (let* ((answer (call-with-current-continuation
                  (lambda (error)
                    (filter stream error)))))
    (and (not (null? answer)) answer)))

; <filter> ::= <empty> ....
(define (filter stream error)
  (if (equal? (peek stream) (integer->char 0))
      '()
      (begin
        (if (skip? (peek stream))
            (begin
              (next stream)
              (filter stream error))
            (cons (ans stream error) (filter stream error))))))

(define (char-sign? char)
  (or (equal? char #\+) (equal? char #\-)))

; <ans> ::= <sign> <rest> |
;           <rest>
; (ans stream error) -> (- rest stream error)) | (rest stream error)
(define (ans stream error)
  (let ((current (peek stream)))
    (and (char-sign? current) (next stream))
    (define ans (rest stream error))
    (if (equal? current #\-)
        (- ans)
        ans)))


; <rest> ::= <number> '/' <number>
;
; (rest stream error) -> (number stream error) / (number stream error)
(define (rest stream error)
  (define num1 (number stream error))
  (and (not (equal? (peek stream) #\/)) (error #f))
  (next stream)
  (define num2 (number stream error))
  (/ num1 num2))



; <number> ::= DIGIT <tail>
(define (number stream error)
  (if (char-numeric? (peek stream))
      (list->integer (cons (next stream) (tail stream error)))
      (error #f)))

; <tail> ::= <empty> | DIGIT <tail>
(define (tail stream error)
  (if (char-numeric? (peek stream))
      (cons (next stream) (tail stream error))
      '()))