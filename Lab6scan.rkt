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


(define (scan-frac str)
  (define stream
    (make-stream (string->list str) (integer->char 0)))
  
  (let* ((result
          (call-with-current-continuation
           (lambda (error)
             (ans stream error)))))
    (and (equal? (peek stream) (integer->char 0)) result)))

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
      (if (not (or (equal? (peek stream) #\/) (equal? (peek stream) (integer->char 0))))
          (error #f)
          '())))
