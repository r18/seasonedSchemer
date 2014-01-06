(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; chapter 11 

(define member?
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     (else (or (eq? a (car lat))
	       (member? a (cdr lat)))))))

(define two-in-a-row? 
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (or (is-first? (car lat) (cdr lat))
	  (two-in-a-row? (cdr lat)))))))
(define is-first? 
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     (else (eq? (car lat) a)))))

(define two-in-a-row-b?
  (lambda (lat)
    (cond 
     ((null? lat) #f)
     (else 
      (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (two-in-a-row-b? lat))))))

(define two-in-a-row-c?
  (lambda (preceeding lat)
    (cond 
     ((null? lat) #f)
     (else (or (eq? (car lat) preceeding)
	       (two-in-a-row-c? (car lat) (cdr lat)))))))

(define two-in-a-row-r?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else (two-in-a-row-c? (car lat) (cdr lat))))))

(define sum-of-prefixes 
  (lambda (tup)
    (cond
     ((null? tup) '())
     (else 

      ))))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
     ((null? tup) '())
     (else (cons (+ sonssf (car tup))
		 (sum-of-prefixes-b
		  (+ sonssf (car tup))
		  (cdr tup)))))))

(define pick 
  (lambda (n lat)
    (cond
     ((eq? 1 n) (car lat))
     (else (pick (- n 1) (cdr lat))))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
     ((null? tup) '())
     (else
      (cons (pick (car tup)
		  (cons (car tup) rev-pre))
	    (scramble-b (cdr tup)
			(cons (car tup) rev-pre)))))))

;; chapter 12

(define Y
  (lambda (le)
     ((lambda (f) (f f))
      (lambda (f)
	(le (lambda (x) ((f f) x)))))))

(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
	  (lambda (lat)
	    (cond
	     ((null? lat) '())
	     ((eq? a (car lat))
	      (mr (cdr lat)))
	     (else (cons (car lat)
			 (mr (cdr lat))))))))
     lat)))
       
(define multirember-b 
  (lambda (a lat)
    ((letrec
	 ((mr (lambda (lat)
		(cond
		 ((null? lat) '())
		 ((eq? a (car lat))
		  (mr (cdr lat)))
		 (else 
		  (cons (car lat)
			(mr (cdr lat))))))))
     mr)
    lat)))


		 
