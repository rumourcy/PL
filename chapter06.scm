(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))

;; (define value
;;   (lambda (nexp)
;;     (cond
;;      ((atom? nexp) nexp)
;;      ((eq? (car (cdr nexp)) '+)
;;       (+ (value (car nexp))
;; 	 (value (car (cdr (cdr nexp))))))
;;      ((eq? (car (cdr nexp)) '*)
;;       (* (value (car nexp))
;; 	 (value (car (cdr (cdr nexp))))))
;;      (else
;;       (pow (value (car nexp))
;; 	   (value (car (cdr (cdr nexp)))))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) '+)
      (+ (value (car (cdr nexp)))
	 (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) '*)
      (* (value (car (cdr nexp)))
	 (value (car (cdr (cdr nexp))))))
     (else
      (pow (value (car (cdr nexp)))
	   (value (car (cdr (cdr nexp)))))))))
