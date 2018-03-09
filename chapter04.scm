(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (add n (sub1 m)))))))

(define sub
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (sub n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (add (car tup) (addtup (cdr tup)))))))

(define multi
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else
      (add n (multi n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (add (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2)))))))

(define gt
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (gt (sub1 n) (sub1 m))))))

(define lt
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (lt (sub1 n) (sub1 m))))))

;; (define eq
;;   (lambda (n m)
;;     (cond
;;      ((and (zero? n) (zero? m)) #t)
;;      ((or (zero? n) (zero? m) #f))
;;      (else
;;       (eq (sub1 n) (sub1 m))))))

;; (define eq
;;   (lambda (n m)
;;     (cond
;;      ((zero? m) (zero? n))
;;      ((zero? n) #f)
;;      (else
;;       (eq (sub1 n) (sub1 m))))))

(define eq
  (lambda (n m)
    (cond
     ((gt n m) #f)
     ((lt n m) #f)
     (else #t))))

(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else
      (multi n (pow n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else
      (add1 (div (sub n m) m))))))

(define len
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else
      (add1 (len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

;; (define rempick
;;   (lambda (n lat)
;;     (cond
;;      ((zero? (sub1 n)) (cdr lat))
;;      (else
;;       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else
      (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
       (else
	(all-nums (cdr lat))))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (eq a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? (car lat) a) (add1 (occur a (cdr lat))))
     (else
      (occur a (cdr lat))))))

;; (define one?
;;   (lambda (n)
;;     (cond
;;      ((zero? n) #f)
;;      (else
;;       (zero? (sub1 n))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else
      (cons (car lat)
	    (rempick (sub1 n) (cdr lat)))))))
