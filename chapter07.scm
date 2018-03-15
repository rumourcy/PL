(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else
      (cond
       ((member? (car lat) (cdr lat)) #f)
       (else
	(set? (cdr lat))))))))
       
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else
      (cons (car lat)
	    (makeset
	     (multirember (car lat)
			  (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (cond
       ((member? (car set1) set2)
	(subset? (cdr set1) set2))
       (else
	#f))))))

(define eqset?
  (lambda (set1 set2)
    (cond
     ((subset? set1 set2)
      (subset? set2 set1))
     (else #f))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((member? (car set1) set2) #t)
     (else
      (intesect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1)
	    (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)
	    (union (cdr set1) set2))))))
