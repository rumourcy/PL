(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

;; (define member?
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) #f)
;;      (else (cond
;; 	    ((eq? a (car lat)) #t)
;; 	    (else (member? a (cdr lat))))))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or
	    (eq? a (car lat))
	    (member? a (cdr lat)))))))
