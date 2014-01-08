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

(define multirember-f 
  (lambda (test?)
    (letrec
	((m-f 
	  (lambda (a lat)
	    (cond 
	     ((null? lat) '())
	     ((test? (car lat) a)
	      (m-f a (cdr lat)))
	     (else 
	      (cons (car lat)
		    (m-f a (cdr lat))))))))
	 m-f)))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
		 (union (cdr set1) set2))))))

(define union-l
  (lambda (set1 set2)
    (letrec
	((U (lambda (set)
	      (cond
	       ((null? set) set2)
	       ((member? (car set) set2)
		(U (cdr set)))
	       (else (cons (car set)
			   (U (cdr set)))))))))))

		
(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1) 
	    (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

(define intersect-l
  (lambda (set1 set2)
    (letrec
	((I (lambda (set)
	      (cond
	       ((null? set) '())
	       ((member? (car set) set2)
		(cons (car set) (I (cdr set))))
	       (else
		(I (cdr set)))))))
      (I set1))))

(define intersectall
  (lambda (lset)
    (cond
     ((null? (cdr lset)) (car lset))
     (else (intersect (car lset)
		      (intersectall (cdr lset)))))))
	 

(define intersectall-l
  (lambda (lset)
    (letrec
	((A (lambda (lset)
	    (cond
	     ((null? (cdr lset))
	      (car lset))
	     (else (intersect (car lset)
			      (A (cdr lset))))))))
      (cond
       ((null? lset) '())
       (else (A (cdr lset)))))))

(define intersectall-c
  (lambda (lset)
    (let/cc hop
	   (letrec
	       ((A (lambda (lset)
		     (cond
		      ((null? (car #?=lset))
		       (hop '()))
		      ((null? (cdr lset))
		       (car lset))
		      (else
		       (intersect (car lset)
				  (A (cdr lset))))))))
	   (cond
	    ((null? lset) '())
	    (else (A lset)))))))


(define intersectall-cc
  (lambda (lset)
    (let/cc hop
	   (letrec
	       ((A (lambda (lset)
		     (cond
		      ((null? (car lset))
		       (hop '()))
		      ((null? (cdr lset))
		       (car lset))
		      (else (I (car lset)
			       (A (cdr lset)))))))
		(I (lambda (s1 s2)
		     (letrec
			 ((J (lambda (s1)
			       (cond
				((null? s1) s2)
				((member? (car s1) s2)
				 (J (cdr s1)))
				(else (cons (car s1)
					    (J (cdr s1))))))))
		       (cond
			((null? s2) (hop '()))
			(else (J s1)))))))
	     (cond
	      ((null? lset) '())
	      (else (A lset)))))))

(define rember-r
  (lambda (a lat)
    (letrec
	((R (lambda (lat)
	      (cond
	       ((null? lat) '())
	       ((eq? (car lat) a)
		(R (cdr lat)))
	       (else (cons (car lat)
			   (R (cdr lat))))))))
	 (R lat))))

(define rember-beyond-first 
  (lambda (a lat)
    (letrec
	((R (lambda (lat)
	      (cond 
	       ((null? #?=lat) '())
	       ((eq? (car lat) a)
		'())
	       (else
		(cons (car lat) (R (cdr lat))))))))
      (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
	    (letrec
		((R (lambda (lat)
		      (cond
		       ((null? lat) '())
		       ((eq? (car lat) a)
			(skip (R (cdr lat))))
		       (else (cons (car lat)
				   (R (cdr lat))))))))
	      (R lat)))))

;;chapter 14
	

