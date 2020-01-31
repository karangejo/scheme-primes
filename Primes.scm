(define (makelist n)                                        ;make a list starting from 2
		(cond ((= n 2) (list 2))
		      ((> n 0) (cons n (makelist (- n 1))))))
(define (makelistm n)                          ;reverse the list
		(reverse (makelist n)))

(define (multipleof? x n)                           ;predicate to check if n is multiple of x
	(if (= (remainder n x) 0) #t
		#f))

(define (rmvmultiplesof n lis)                                       ;remove mulitples of n from lis
	(cond ((null? lis) '())
	      ((= (remainder (car lis) n) 0) (rmvmultiplesof n (cdr lis)))
	      ((> (remainder (car lis) n) 0) (cons (car lis) (rmvmultiplesof n (cdr lis))))))

(define (multofthree? n)                  ;not used
	(multipleof? 3 n))

(define (primes lis)                            ;just keep the primes from the lis. This function implements sieves
	(cond ((null? lis) '())
      	      (#t (cons (car lis) (primes (rmvmultiplesof (car lis) (cdr lis)))))))

(define (primeslessthan x)                     ;the main function the user should call to find all primes less than or equal to x
	(let ((lis (makelistm x))) 
		(primes lis)))

(define (makelistofmultiplesof x n)          ;make a list of multiples of x less than or equal to n
	(if (> (remainder n x) 0) 
		(makelistofmultiplesof x (- n 1))
		(cond ((= n 0) '())
		      ((> n 0) (cons n (makelistofmultiplesof x (- n x)))))))

	

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cartesian product;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define cartprod                         ;cartesian product of two lists
	(lambda (lis1 lis2)
	(cond ((null? lis1) '())
		(else (append (map-to-lis (car lis1) lis2) (cartprod (cdr lis1) lis2))))))

(define map-to-lis                 ; helper (like mapcar)
	(lambda (x lis)
	(cond ((null? lis) '())
		((pair? x) (cons (append x (list (car lis))) (map-to-lis x (cdr lis))))
		(else (cons (cons x (list (car lis))) (map-to-lis x (cdr lis)))))))

(define cartesianprod                 ;main user function used  input is a list of lists like this: (cartesianprod '((1 2 3) (a b c)))
	(lambda (lisoflis)
	(cond ((= (length lisoflis) 1) (car lisoflis))
		(else (cartesianprod (append  (list (cartprod (car lisoflis) (cadr lisoflis))) (cddr lisoflis)))))))

