;; 1.1.7

;; sqrt-iter 
(define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x)
				   x)))

;; improve
(define (improve guess x)
	(average guess (/ x guess)))
;;average
(define (average x y)
	(/ (+ x y) 2))

;; good-enough?
(define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))

;; sqrt guess begin with 1.0
(define (sqrt x)
	(sqrt-iter 1.0 x))

;; excries 1.7
(define (good-enough? guess x)
	(< (/ (abs (- (improve guess x)
				  guess))
		  guess)
	   0.00001))
		
;; excries 1.8
(define (three-sqrt guess x)
	(if (good-enough3? guess x)
		guess
		(three-sqrt (improve3 guess x) 
					x)))

(define (good-enough3? guess x)
	(< (/ (abs (- (improve3 guess x)
				  guess))
		  guess)
	   0.00001))

(define (improve3 guess x)
	(/ (+ (/ x (square guess))
		  (* guess 2.0))
	   3.0))

(define (sqrt3 x)
	(three-sqrt 1.0 x))

;;;;;;;;;;;Counting chage
(define (count-change amount)
	(cc amount 5))

(define (cc amount kinds-of-coins)
	(cond ((= amount 0) 1)
		  ((or (< amount 0) (= kinds-of-coins 0)) 0)
		  (else
			(+ (cc amount
				   (- kinds-of-coins 1))
			   (cc (- amount
					  (first-denomination kinds-of-coins))
				   kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
		((= kinds-of-coins 2) 5)
		((= kinds-of-coins 3) 10)
		((= kinds-of-coins 4) 25)
		((= kinds-of-coins 5) 50)))

;; exerice1.11
(define (F n)
	(if (< n 3)
		n	
		(+ (F (- n 1))
		   (* 2 (F (- n 2)))
		   (* 3 (F (- n 3))))))

(define (F-iterative n)
	(define (F-iter a b c n)
		(if (= n 3)
			(+ a (* 2 b) (* 3 c))
			(F-iter (+ a (* 2 b) (* 3 c))
					a b (- n 1))))
					
  (if (< n 3)
	  n
	  (F-iter 2 1 0 n)))

;; exerice 1.12
;; x is lines,	y is set. x and y beginning from 1
(define (yanghui x y)
	(cond ((< x y)
		  #f)
		  (else
			(if (or (= y 1) (= x y))
				1
				(+ (yanghui (- x 1) y)
				   (yanghui (- x 1) (- y 1)))))))


;;1.2.4
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
	  product
	  (expt-iter b
				(- counter 1)
				(* b product)))) 

(define (fast-expt b n)
  (cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

;; exerice 1.16
(define (expt-iter b n)
	
  (define (expt-it n a c)
	  (cond
		((= n 1) (* c a))
		((even? n) (expt-it (/ n 2) (* a a) c))
		(else
		  (expt-it (- n 1) a (* a c)))))
  
  (cond ((= n 0) 1)
		((= n 1) b)
		(else
		  (expt-it n b 1))))

;; exerice 1.18
(define (mulit a b)
	(define (mulit-ite b c x)
	  (cond ((= b 0) 0)
			((= b 1) (+ c x))
			((even? b) (mulit-ite (/ b 2) (* 2 c) x))
			(else
			  (mulit-ite (- b 1) c (+ c x)))))
  
  (cond ((or (and (< a 0) (> b 0))
			(and (> a 0) (< b 0)))
		 (- (mulit-ite (abs b) (abs a) 0)))
		(else
		  (mulit-ite (abs b) (abs a) 0))))


;; 1.2.6
(define (prime? n)
	   (define (smallest-divisor n)
		   (find-divisor n 2))
	   
	   (define (find-divisor n test-divisor)
		   (cond ((> (square test-divisor)
					 n) n)
				 ((divides? test-divisor n) test-divisor)
				 (else
				   (find-divisor n (+ test-divisor 1)))))
	   
	   (define (divides? a b)
		   (= (remainder b a) 0))
	   
	   (= n (smallest-divisor n)))

;;费马检测 实现一个计算一个数的幂对另一个数的摸运算。
(define (expmod base exp m)
	(cond ((= exp 0) 1)
		  ((even? exp)
		   (remainder (square (expmod base (/ exp 2) m))
					  m))
		  (else
			(remainder (* base (expmod base (- exp 1) m))
					   m))))

(define (expmod base exp m)
	(remainder (fast-expt base exp)
			   m))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
  
  (try-it (+ 1 (random (- n 1)))))

;; 测试一个数 x次。
(define (fast-prime? n times)
	(cond ((= times 0) #t)
		  ((fermat-test n) (fast-prime? n (- times 1)))
		  (else #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;exerise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
	  (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes begining end)
	(define (primes-element val end)
		(cond ((> val end)
			   (display "over"))
			  (else
				 (timed-prime-test val)
				 (primes-element (+ 2 val) end))))
  
  (if (even? begining)
	  (primes-element (+ begining 1) end)
	  (primes-element begining end)))
	  
;; exerise 1.23
(define (next test-divisor)
	(if (= 2 test-divisor)
		3
		(+ 2 test-divisor)))


;; 1.3.1
(define (cube x)
	(* x x x))

(define (inc n) 
	(+ n 1))
;; 求和概念
(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
		   (sum term (next a) next b))))

(define (sum-cubes a b)
	(sum cube a inc b))


;; define pi-sum
(define (pi-sum a b)
	
	(define (pi-term x)
		(/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
	  (+ x 4))
  
  (sum pi-term a pi-next b))

;; define integral
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
	 dx))

;; exerice 1.29
(define (xps a b n)

	(define (get-h)
		(/ (- b a) n))
  
	(define (xps-next x)
		   (+ 1 x))
  
  ;; f
	(define (xps-term x)
		   (define (ps-term-get-y)
			   (+ a (* x (get-h))))
		
		   (cond ((= x 0) (ps-term-get-y))
				 ((= x n) (ps-term-get-y))
				 ((even? x) (* 2.0 (cube (ps-term-get-y))))
				 (else
				   (* 4.0 (cube (ps-term-get-y))))))

	(define (xps-implement h)
		   (* (/ h 3.0)
			  (sum xps-term 0 next n)))
	
	(xps-implement (get-h)))

;; exerice 1.31
(define (product term a next b)
	(if (> a b)
		1
		(* (term a)
		   (product term (next a) next b))))

(define (pi n)
	(define (pro-term-fz x)
		(if (even? x)
			(+ x 2)
			(+ x 3)))
  
	(define (pro-term-fm x)
		 (if (even? x)
			 (+ x 3)
			 (+ x 2)))
  (define (pro-next x) (+ 1 x))
	(* 4.0
	   (/ (product pro-term-fz 0 pro-next n)
		  (product pro-term-fm 0 pro-next n))))

;; exerice 1.32
(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a)
				  (accumulate combiner null-value term (next a) next b))))
;; (b) 迭代
(define (accumulate combiner null-value term a next b)
	(define (acc-pro k result)
		(if (> k b)
			result
			(acc-pro (next k) (combiner (term a)
										(result)))))
		
  (acc-pro a null-value))


;;;;
(define (f x y)
	((lambda (a b)
			 (+ (* x (square a))
				(* y b)
				(* a b)))
	 (+ 1 (* x y))
	 (- 1 y)))
			 
(define (f x y)
	(let ((a (+ 1 (* x y)))
		  (b (- 1 y)))
		 (+ (* x (square a))
			(* y b)
			(* a b))))


;; 1.3.3
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
	(if (close-enough? neg-point pos-point)
		midpoint
		(let ((test-value (f midpoint)))
		  (cond ((positive? test-value)
				 (search f neg-point midpoint))
				((negative? test-value)
				 (search f midpoint pos-point))
				(else midpoint))))))

(define (close-enough? x y)
	(< (abs (- x y)) 0.0001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
		(b-value (f b)))
	(cond ((and (negative? a-value) (positive? b-value))
		   (search f a b))
		  ((and (negative? b-value) (positive? a-value))
		   (search f b a))
		  (else
		   (error "Values are not of opposite sign" a b)))))


;; 找不动点
(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		   (let ((next (f guess)))
				(newline)
				(display next)
				(if (close-enough? guess next)
					next
					(try next))))
  (try first-guess))

(define (sqrtt x)
	(fixed-point (lambda (y) (average y (/ x y)))
				 1.0))

;; exerice 1.35
(define (gold-point)
	(fixed-point (lambda (y) (+ 1 (/ 1 y)))
				 1.0))

;; exerice 1.36
(define (get-log)
	(fixed-point (lambda (y) (/ (log 1000) (log y)))
				 3.0))
;; 采用阻尼技术
(define (get-log)
	(fixed-point (lambda (y) (average y (/ (log 1000) (log y))))
				 3.0))

;; exerice 1.37
;; 递归计算时top-donw
(define (cont-frac n d k)
	
	(define (cont-frac-per i)
		(if (= k i)
		    (/ (n i) (d i))
			(/ (n i)
			   (+ (d i) (cont-frac-per (+ i 1))))))
			
  (cont-frac-per 1))
			
;; 迭代计算时butten-to-top
(define (cont-frac n d k)
	
	(define (cont-frac-per i total)
		(if (= 0 i)
			total
			(cont-frac-per (- i 1)
						   (+ (d i)
							  (/ (n i) total)))))
			
  (cont-frac-per k 1.0))

;; exerice 1.38
(define (get-e k)
  (+ 2
	  (cont-frac (lambda (i) 1.0)
				 (lambda (i) 
						 (cond ((= (remainder (- i 2) 3) 0) 
								(+ 2.0 
									(* 2 (/ (- i 2) 3))))
								(else
								  1.0)))
				 k)))

;; exerice 1.39
(define (tan-cf x k)
  (cont-frac tan-n 
	  (lambda (i) 
		(if (= 1 i)
			x
			(- (* x x))))
	  (lambda(i) (- (* i 2.0) 1.0)) k))									   
