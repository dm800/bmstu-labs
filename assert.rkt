(define (assert expr)
  (and (not (eval expr (interaction-environment))) (exit)))