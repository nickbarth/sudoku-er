(use srfi-1)

; make-board :-> [][]int
(define (make-board)
  `((0 0 0 0 0 0 0 0 9)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0)))

; test : make-board
(define (test-make-board)
  (unless (equal? (length (make-board)) 9)
    (print "ERROR - board should be 9x9"))
  (unless (equal? (length (car (make-board))) 9)
    (print "ERROR - board should be 9x9")))
(test-make-board)

; set-row : []int int []row -> []int
(define (set-row board n row)
  (append (take board n) (list row) (drop board (+ n 1))))

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

; set-position : [][]int int int int -> [][]int
(define (set-position board x y val)
  (let ((row (get-row board y)))
   (append (take board y) (list (set-row row x val)) (drop board (+ y 1)))))

; test : set-position
(define (test-set-position)
  (let ((board '((0 0 0) (0 0 0))))
    (unless (equal? (set-position board 1 1 1) '((0 0 0) (0 1 0)))
      (print "ERROR - get-column incorrect"))))
(test-set-position)

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
      (cons n r))) '() '(1 2 3 4 5 6 7 8 9)))

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

; rows-solved? : [][]int -> bool
(define (rows-solved? board)
  (fold (lambda (row r)
    (and (equal? (sort row <) '(1 2 3 4 5 6 7 8 9)) r))
      #t board))

; test : rows-solved?
(define (test-rows-solved)
  (let ((board `((1 2 3 4 5 6 7 8 9)
                 (1 2 3 4 5 6 7 8 9)
                 (1 2 3 4 5 6 7 8 9)
                 (1 2 3 4 5 6 7 8 9)
                 (1 2 3 4 5 6 7 8 9)
                 (1 2 3 4 5 6 7 8 9)
                 (1 2 3 4 5 6 7 8 9)
                 (1 2 3 4 5 6 7 8 9)
                 (1 2 3 4 5 6 7 8 9))))
    (unless (rows-solved? board)
      (print "ERROR - rows-solved? incorrect"))))
(test-rows-solved)

; cols-solved? : [][]int -> bool
(define (cols-solved? board)
  (fold (lambda (col r)
    (and (equal? (sort (get-column board col) <) '(1 2 3 4 5 6 7 8 9)) r))
      #t '(0 1 2 3 4 5 6 7 8)))

; test : cols-solved?
(define (test-cols-solved)
  (let ((board `((1 1 1 1 1 1 1 1 1)
                 (2 2 2 2 2 2 2 2 2)
                 (3 3 3 3 3 3 3 3 3)
                 (4 4 4 4 4 4 4 4 4)
                 (5 5 5 5 5 5 5 5 5)
                 (6 6 6 6 6 6 6 6 6)
                 (7 7 7 7 7 7 7 7 7)
                 (8 8 8 8 8 8 8 8 8)
                 (9 9 9 9 9 9 9 9 9))))
    (unless (cols-solved? board)
      (print "ERROR - cols-solved? incorrect"))))
(test-cols-solved)

; quads-solved? : [][]int -> bool
(define (quads-solved? board)
  (fold (lambda (quad r)
    (and (equal? (sort (get-quad board (car quad) (cadr quad)) <) '(1 2 3 4 5 6 7 8 9)) r))
      #t '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))

; test : quads-solved?
(define (test-quads-solved)
  (let ((board `((1 2 3 1 2 3 1 2 3)
                 (4 5 6 4 5 6 4 5 6)
                 (7 8 9 7 8 9 7 8 9)
                 (1 2 3 1 2 3 1 2 3)
                 (4 5 6 4 5 6 4 5 6)
                 (7 8 9 7 8 9 7 8 9)
                 (1 2 3 1 2 3 1 2 3)
                 (4 5 6 4 5 6 4 5 6)
                 (7 8 9 7 8 9 7 8 9))))
    (unless (quads-solved? board)
      (print "ERROR - quads-solved? incorrect"))))
(test-quads-solved)

; is-solved? : [][]int -> bool
(define (is-solved? board)
  (and (rows-solved? board)
       (cols-solved? board)
       (quads-solved? board)))

; has-spaces? : [][]int -> bool
(define (has-spaces? board)
  (if (member 0 (fold append '() board)) #t #f))

; test : has-spaces?
(define (test-has-spaces)
  (let ((board `((1 1 1 1 1 1 1 1 1)
                 (1 1 0 1 1 1 1 1 1))))
    (unless (has-spaces? board)
      (print "ERROR - has-spaces? incorrect"))))
(test-has-spaces)

; find-space [][]int -> []int{ x, y }
(define (find-space board)
  (fold (lambda (y r)
    (let ((x (fold (lambda (n r) (if (eq? (list-ref (list-ref board y) n) 0) n r)) #f '(0 1 2 3 4 5 6 7 8))))
      (if x (list x y) r))) #f '(0 1 2 3 4 5 6 7 8)))

; test : find-space
(define (test-find-space)
  (let ((board `((1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 1 1)
                 (1 1 1 1 1 1 1 0 1)
                 (1 1 1 1 1 1 1 1 1))))
    (unless (equal? (find-space board) '(7 7))
      (print "ERROR - find-space incorrect"))))
(test-find-space)

; solve [][]int -> [][]int || #f
(define (solve board)
  (cond
    ((is-solved? board) board)
    ((not (has-spaces? board)) #f)
    ((has-spaces? board)
      (let ((pos (find-space board)))
        (fold (lambda (n r)
          (let ((test-board (set-position board (car pos) (cadr pos) n)))
            (or (solve test-board) r)))
          #f (avaliable-at-position board (car pos) (cadr pos)))))))

(define (main)
  (let ((board
   '((0 0 2 0 0 0 5 0 0)
     (0 1 0 7 0 5 0 2 0)
     (4 0 0 0 9 0 0 0 7)
     (0 4 9 0 0 0 7 3 0)
     (8 0 1 0 3 0 4 0 9)
     (0 3 6 0 0 0 2 1 0)
     (2 0 0 0 8 0 0 0 4)
     (0 8 0 9 0 2 0 6 0)
     (0 0 7 0 0 0 8 0 0))))
  (pp board)
  (print "-----")
  (pp (solve board))))
(main)
