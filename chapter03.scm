(define rember
  (lambda (a lat)
    (cond
     ((null? lat) lat)
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat) (rember? a (cdr lat)))))))
