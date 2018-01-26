(define (chunk xs n)
  (if (pair? xs) (cons (take xs n) (chunk (drop xs n) n)) '() ))
(chunk '(1 2 3 4 5 6) 3)

(define (take xs n)
  (if (zero? n)
    '() (cons (car xs) (take (cdr xs) (- n 1)))))
(take '(1 2 3 4 5 6) 3)

(define (drop xs n)
  (if (zero? n) xs (drop (cdr xs) (- n 1))))
(drop '(1 2 3 4 5 6) 3)

(define (fold func init xs)
  (if (null? xs)
    init (func (car xs) (fold func init (cdr xs)))))

(fold (lambda (x r) (cons x r)) '() '(1 2 3))

; get-column : [][]int int -> []int
(define (get-column board n)
  (fold (lambda (col r)
    (append r (take (drop col n) 1))) '() board))

; test : get-column
(define (test-get-column)
  (let ((board '((0 0 0)(0 1 0)(0 0 0))))
    (unless (equal? (get-column board 1) '(0 1 0))
      (print "ERROR - get-column incorrect"))))
(test-get-column)

(define fold
  (lambda (l result fun)
    (cond
      ((null? l) result)
      (else
        (fold (cdr l) (fun result (car l)) fun)))))



(define fold
  (lambda (l init fun)
    (cond
      ((null? (cdr l)) (fun init (car l)))
      (else
        (fun (fold (cdr l) init fun) (car l))))))

(define (reduce fn init xs)
  (if (null? xs) init
    (fn (car xs)
      (reduce fn init (cdr xs)))))

(define (fold fn init xs)
  (if (null? xs) init
    (fn (car xs)
      (fold fn init (cdr xs)))))


(define (fold fn init xs)
  (if (null? xs) init
    (fn (car xs)
      (fold fn init (cdr xs)))))

(has-1-9? '(2 1 3 4 5 6 7 8 9))

  (let ((1-9 '(1 2 3 4 5 6 7 8 9)))
    (equal? 1-9 (fold (lambda (n r)
      (if (member n nums) (append r (list n)) r)) '() 1-9))))

; test : get-quad
(define (test-get-quad)
  (let ((board '((1 2 3 0 0 0 0 0 0) (4 5 6 0 0 0 0 0 0 0 0 0) (7 8 9 0 0 0 0 0 0 0 0 0))))
    (get-quad board 0 0)))
(test-get-quad)
