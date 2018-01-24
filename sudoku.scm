(use srfi-1)

; make-board :-> [][]int
(define (make-board)
  `((0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)))

; avaliable :-> []int
(define (avaliable)
  '(1 2 3 4 5 6 7 8 9))

; test : make-board
(define (test-make-board)
  (unless (equal? (length (make-board)) 9)
    (print "ERROR - board should be 9x9"))
  (unless (equal? (length (car (make-board))) 9)
    (print "ERROR - board should be 9x9")))
(test-make-board)

; set-row : [][]int int []row -> [][]int
(define (set-row board n row)
  (append 
    (append (take board n) (list row))
      (drop board (+ n 1))))

; test : set-row
(define (test-set-row)
  (unless (equal? (cadr (set-row (make-board) 1 '(5))) '(5))
    (print "ERROR - row not set")))
(test-set-row)

; get-row : [][]int int -> []int
(define (get-row board n)
  (list-ref board n))

; test : get-row
(define (test-get-row)
  (let ((board (set-row (make-board) 2 '(1 1 1))))
    (unless (equal? (get-row board 2) '(1 1 1))
      (print "ERROR - get-row incorrect"))))
(test-get-row)

; get-column : [][]int int -> []int
(define (get-column board n)
  (fold (lambda (col r)
    (append r (take (drop col n) 1))) '() board))

; test : get-column
(define (test-get-column)
  (let ((board (set-row (make-board) 1 '(1 1))))
    (unless (equal? (get-column board 1) '(0 1 0 0 0 0 0 0 0))
      (print "ERROR - get-column incorrect"))))
(test-get-column)

; get-quad : [][]int int int -> []int
(define (get-quad board x y)
  (let ((section (take (drop board (* y 3)) 3)))
    (fold (lambda (col r)
      (append r (take (drop col (* x 3)) 3))) '() section)))
(get-quad (make-board) 2 2)

; test : get-quad
(define (test-get-quad)
  (let ((board (set-row (make-board) 3 '(0 0 0 1 2 3))))
    (unless (equal? (get-quad board 1 1) '(1 2 3 0 0 0 0 0 0))
      (print "ERROR - get-quad incorrect"))))
(test-get-quad)

; could-be : []int -> []int
(define (could-be nums)
  (fold (lambda (n r)
    (if (member n nums) r
      (cons n r))) '() (avaliable)))

; test : could-be
(define (test-could-be)
  (unless (equal? (could-be '(1 2 3)) '(9 8 7 6 5 4))
    (print "ERROR - could-be incorrect")))
(test-could-be)

; avaliable-at-position : [][]int int int -> []int
(define (avaliable-at-position board x y)
  (could-be (append
    (get-column board x)
    (get-row board y)
    (get-quad board (inexact->exact (floor (/ x 3)))
                    (inexact->exact (floor (/ y 3)))))))

; test : avaliable-at-position
(define (test-available-at-position)
  (let ((board (set-row (make-board) 0 '(0 1 2 3 4 6 7 8 9))))
    (unless (equal? (avaliable-at-position board 0 0) '(5))
      (print "ERROR - get-quad incorrect"))))
(test-available-at-position)


; get-something : [][]int int -> []int
(define (available-something board n) 
  (print "ERROR - not implemented"))

(define (test-get-something)
  (print "ERROR - not implemented"))
