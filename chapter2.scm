;; 2.1.1

;; gcd come from 1.25
(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
	(cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;; exerice 2.1
(define (make-rat n d)
		(let ((rat (cond ((or (and (< n 0) (< d 0))
							  (and (> n 0) (< d 0)))
						  (cons (* n -1) (* d -1)))
						 (else
						   (cons n d)))))
			 (let ((rat-n (car rat))
				   (rat-d (cdr rat)))
				  (let ((g (gcd rat-n rat-d)))
					   (cons (/ rat-n g) (/ rat-d g))))))
			 
;; 别人写的。抓住了问题的实质。
(define (make-rat x y)
  (let ((g (gcd x y)))
	(if (< y 0)
		(cons (/ (- x) g) (/ (- y) g))
		(cons (/ x g) (/ y g))))) 


;; exerice 2.2
(define (make-point x y)
	(cons x y))
(define (x-point line)
	(car line))
(define (y-point line)
	(cdr line))

(define (make-segment a b)
	(cons a b))
(define (start-segment s)
	(car s))
(define (end-segment s)
	(cdr s))

(define (midpoint-segment s)
	(make-point
		(/ (+ (x-point (end-segment s))
			  (x-point (start-segment s))) 2.0)
	  (/ (+ (y-point (end-segment s))
			(y-point (start-segment s))) 2.0 )))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
  
  
;; exerice 2.2
;;基于长和高的表示
(define (make-rect l h)
	(cons l h))

(define (get-length rect)
	(car rect))
(define (get-hight rect)
	(cdr rect))

;; 屏蔽曾 下面的计算基于rect的表示
(define (circumference rect)
	(* 2 (+ (get-length rect)
			(get-hight rect))))

(define (area rect)
	(* (get-length rect)
	   (get-hight rect)))
  
;; exercise 2.4
(define (cons x y)
	(lambda (m) (m x y)))
(define (cdr z)
	(z (lambda (p q) q)))

;; exercise 2.5
;; 除到只省2^a or 3^b 为止。
(define (cons a b) (* (expt 2 a) (expt 3 b)))

(define (fact-n x c a)
	(if (> (remainder x c) 0)
		a
		(fact-n (/ x c) c (+ a 1))))
(define (car z) (fact-n z 2 0))
(define (cdr z) (fact-n z 3 0))

;; exercise 2.6 没想出来
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; exercise 2.7 
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
				 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
		(p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	(make-interval (min p1 p2 p3 p4)
				   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
				(make-interval (/ 1.0 (upper-bound y))
							   (/ 1.0 (lower-bound y)))))
(define (make-interval a b) (cons a b))
(define (upper-bound a) (cdr a))
(define (lower-bound a) (car a))

;; exercise 2.8
(define (sub-interval a b)
	(make-interval (- (upper-bound x) (lower-bound y))
				   (- (lower-bound x) (upper-bound y))))

;; exercise 2.10
(define (div-interval x y)
	(let ((y-l (lower-bound y))
		  (y-u (upper-bound y)))
		 (if (< (* y-l y-u))
			 (error "divisor interval expands zero")
			 (mul-interval x
						   (make-interval
							   (/ 1.0 y-u)
							 (/ 1.0 y-l))))))

;; exercise 2.11
(define (mul-interval x y)
	(let ((l-x (lower-bound x))
		  (u-x (upper-bound x))
		  (l-y (lower-bound y))
		  (u-y (upper-bound y)))
		 (cond ((> l-x 0)
				(cond ((> l-y 0) (make-interval (* l-x l-y) (* u-x u-y))) ;;++
					  ((< u-y 0) (make-interval (* u-x l-y) (* l-x u-y))) ;;--
					  (else (make-interval (* u-x l-y) (* u-x u-y)))))
			   ((< u-x 0)
				(cond ((> l-y 0) (make-interval (* l-x u-y) (* u-x l-y))) ;;++
					  ((< u-y 0) (make-interval (* u-x u-y) (* l-x l-y))) ;;--
					  (else (make-interval (* u-x u-y) (* l-x l-y)))))
			   (else (cond ((> l-y 0) (make-interval (* l-x u-y) (* u-x u-y))) ;;++
							((< u-y 0) (make-interval (* u-x l-y) (* l-x l-y))) ;;--
							 (else (make-interval 
											 (min (* u-x l-y) (* l-x u-y))
											 (max (* l-x l-y) (* u-x u-y)))))))))
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
;; exercise 2.12
(define (make-center-percent c p)
	(make-interval (- c (* c p))
				   (+ c (* c p))))
(define (precent x)
	(/ (width x) (center x)))

;; exercise 2.17
(define (last-pair l)
	(if (null? (cdr l))
		(car l)
		(last-pair (cdr l))))

;; exercise 2.18
(define (reverse l)
	(cond ((null? (cdr l)) (car l))
		  (else
			(reverse-ite (cons (car l) '())
						 (cdr l)))))
;; s is reversed car l, l is cdr orinal l
(define (reverse-ite s l)
	(if (null? (cdr l))
		(cons (car l) s)
		(reverse-ite (cons (car l) s)
					 (cdr l))))
;; modify 2.18
(define (reverse l)
	(define (reverse-ite s l)
		(if (null? l)
			s
			(reverse-ite (cons (car l) s) (cdr l))))
  (reverse-ite '() l))

;; execrice 2.20
(define (same-parity a . l)
			(if (even? a)
				(cons a (get-even l))
				(cons a (get-odd l))))
(define (get-even  l)
			(cond ((null? l) l)
				  ((even? (car l)) (cons (car l) (get-even (cdr l))))
				  (else
					(get-even (cdr l)))))
(define (get-odd l)
			(cond ((null? l) l)
				  ((odd? (car l)) (cons (car l) (get-odd (cdr l))))
				  (else
					(get-odd (cdr l)))))

;; exercise 2.23
(define (for-each x l)
	(cond ((null? l) #t)
		  (else (x (car l)) (for-each x (cdr l)))))


;;2.22
(define (count-leaves x)
  (cond ((null? x) 0)  
		((not (pair? x)) 1)
		(else (+ (count-leaves (car x))
				 (count-leaves (cdr x))))))

;; exercise 2.27
;; base on the 2.18, differense is test if (car l) is a list 
(define (deep-reverse l)
	(define (reverse-list l s)
		(cond ((null? l) s)
			  ((list? (car l))
			   (reverse-list (cdr l) (cons (reverse-list (car l) '()) s)))
			  (else
				(reverse-list (cdr l) (cons (car l) s)))))
  (reverse-list l '()))


;; exercise 2.28
(define (fringe t)
	(define (fringe-get t s)
		(cond ((null? t) s)
			  ((list? (car t)) 
			   (fringe-get (car t) 
						   (fringe-get (cdr t) s)))
			  (else
				(cons (car t) (fringe-get (cdr t) s)))))
  (fringe-get t '()))

;; from anther auther
(define (fringe x)
  (define (iter tree lst)
	(cond ((null? tree) lst)
		  ((not (pair? tree)) (cons tree lst))
		  (else (iter (car tree) (iter (cdr tree) lst)))))
  (iter x ()))


;; exercise 2.29
(define (make-mobile left right)
	(list left right))
(define (make-branch length structure)
	(list length structure))

;; a)
(define (left-branch t)
	(car t))
(define (right-branch t)
	(car (cdr t)))
(define (branch-length b)
	(car b))
(define (branch-structre b)
	(car (cdr b)))

;; b)
(define (total-weight t)
	(define (get-weight b )
		(cond ((list? (branch-structre b)) 
			   (get-weight (total-weight (branch-structre b))))
			  (else
				(branch-structre b))))
  (+ (get-weight (left-branch t))
	 (get-weight (right-branch t))))

;; c)
(define (test-eq t)
	(let ((l-t (left-branch t))
		  (r-t (right-branch t)))
		 (if (= (* (branch-lenght l-t)
				   (if (list? (branch-structre l-t))
					   (test-eq (branch-structre l-t))
					   (branch-structre l-t)))
				(* (branch-lenght r-t)
				   (if (list? (branch-structre r-t))
					   (test-eq (branch-structre r-t))
					   (branch-structre r-t))))
			 #t
			 #f)))


;; exercise 2.30
(define (square-tree t)
	(cond ((null? t) '())
		  ((not (pair? t)) (* t t))
		  (else
			(cons (square-tree (car t)) 
				  (square-tree (cdr t))))))

(define (square-tree t)
	(map (lambda (t)
				 (if (pair? t)
					 (square-tree t)
					 (* t t)))
	  t))

					   
;; exercise 2.31
(define (tree-map x t)
	(map (lambda (t)
				 (if (pair? t)
					 (tree-map x t)
					 (x t)))
	  t))

;; exercise 2.32
(define (subsets s)
	(if (null? s)
		(list '())
		(let ((rest (subsets (cdr s))))
			 (append rest (map (lambda (x) (cons (car s) x)) rest)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))
;; exercise 2.33
(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append seq1 seq2)
	(accumulate cons seq1 seq2))
(define (length sequence)
	(accumulate (lambda (x y) (+ 1 y)) 0 sequence))


;; exercise 2.34
(define (horner-eval x cofficient-sequence)
	(accumulate (lambda (this-coeff higher-terms) 
						(+ (* x higher-terms)
						   this-coeff)
						0
						coefficient-squence)))

;; exercise 2.35
(define (count-leaves t)
	(accumulate +
				0 
				(map (lambda (x) (if (list? x)
										   (count-leaves x)
										   1))
				  t)))

;; exercise 2.36
(define (accumulate-n op init seqs)
	(if (null? (car seqs))
		'()
		(cons (accumulate op init (map car seqs))
			  (accumulate op init (map cdr seqs)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


;;
(define (enumerate-interval low high)
	(if (> low high)
		'()
		(cons low (enumerate-interval (+ 1 low) high))))
(accumulate append
			'()
			(map (lambda (i)
						 (map (lambda (j)
									  (list i j))
						   (enumerate-interval 1 (- i 1))))
						 (enumerate-interval 1 n)))

(define (flatmap proc seq)
	(accumulate append '() (map proc seq)))
(define (prime-sum? pair)
	(prime? (+ (car pair) (cadr paiar))))
