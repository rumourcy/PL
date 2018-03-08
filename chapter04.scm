(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define ＋
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (＋　n (sub1 m)))))))

(define －
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (－ n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (＋ (car tup) (addtup (cdr tup)))))))

(define ＊
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (＋ n (＊ n (sub1 m)))))))
