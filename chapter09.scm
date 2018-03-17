(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else
      (eq? sorn a)))))
