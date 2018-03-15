;; (define rember-f
;;   (lambda (test? a l)
;;     (cond
;;      ((null? l) '())
;;      (else
;;       (cond
;;        ((test? (car l) a) (cdr l))
;;        (else
;; 	(cons (car l)
;; 	      (rember-f test? a (cdr l)))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else
	(cons (car l) ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
	(cons new (cons old (cdr l))))
       (else
	(cons (car l)
	      ((insert-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
	(cons old (cons new (cdr l))))
       (else
	(cons (car l)
	      ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (test? seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
	(seq new old (cdr l)))
       (else
	(cons (car l) ((insert-g test? seq) new old (cdr l))))))))

(define insertL (insert-g eq? seqL))

(define insertR (insert-g eq? seqR))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g eq? seqS))

(define seqrem
  (lambda (new old l)
    l))

(define rember
  (lambda (a l)
    ((insert-g eq? seqrem) #f a l)))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) add)
     ((eq? x '*) multi)
     (else pow))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (car nexp))
       (value (car (cdr nexp)))
       (value (car (cdr (cdr nexp)))))))))
