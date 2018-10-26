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

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat))
	((mutlirember-f test?) a (cdr lat)))
       (else
	(cons (car lat)
	      ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else
      (cons (car lat) (multiremberT test? (cdr lat)))))))

(define multiremberCo
  (lambda (a lat col)
    (cond
     ((null? lat)
      (col '() '()))
     ((eq? (car lat) a)
      (multiremberCo a (cdr lat)
		     (lambda (newlat seen)
		       (col newlat (cons (car lat) seen)))))
     (else
      (multiremberCo a (cdr lat)
		     (lambda (newlat seen)
		       (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(define last-friend
  (lambda (x y)
    (length x)))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new (cons oldL
		      (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR (cons new
		       (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat)
	    (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLRCo
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLRCo new oldL oldR
		       (cdr lat)
		       (lambda (new lat L R)
			 (col (cons new (cons oldL newlat)) (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLRCo new oldL oldR
		       (cdr lat)
		       (lambda (newlat L R)
			 (col (cons oldR (cons new newlat)) L (add1 R)))))
     (else
      (multiinsertLRCo new oldL oldR
		       (cdr lat)
		       (lambda (newlat L R)
			 (col (cons (car lat) newlat L R))))))))

(define even?
  (lambda (n)
    (eq (multi (div n 2) 2) n)))

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l))
	(cons (car l)
	      (evens-only* (cdr l))))
       (else
	(evens-only* (cdr l)))))
      (else
       (cons (evens-only* (car l))
	     (evens-only* (cdr l))))))))

(define evens-only*Co
  (lambda (l col)
    (cond
     ((null? l)
      (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
	(evens-only*Co (cdr l)
		       (lambda (newl p s)
			 (col (cons (car l) newl) (multi (car l) p) s))))
       (else
	(evens-only*Co (cdr l)
		      (lambda (newl p s)
			(col newl
			     p (add (car l) s)))))))
     (else (evens-only*Co (car l)
			  (lambda (al ap as)
			    (evens-only*Co (cdr l)
					   (lambda (dl dp ds)
					     (col (cons al dl)
						  (multi ap dp)
						  (add as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons sum
	  (cons product newl))))
