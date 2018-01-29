(declare (unit sudoku))

(use srfi-1)

; set-row : []int int []row -> []int
(define (set-row board n row)
  (append (take board n) (list row) (drop board (+ n 1))))

; get-row : [][]int int -> []int
(define (get-row board n)
  (list-ref board n))

; set-position : [][]int int int int -> [][]int
(define (set-position board x y val)
  (let ((row (get-row board y)))
   (append (take board y) (list (set-row row x val)) (drop board (+ y 1)))))

; get-column : [][]int int -> []int
(define (get-column board n)
  (fold (lambda (col r)
    (append r (take (drop col n) 1))) '() board))

; get-quad : [][]int int int -> []int
(define (get-quad board x y)
  (let ((section (take (drop board (* y 3)) 3)))
    (fold (lambda (col r)
      (append r (take (drop col (* x 3)) 3))) '() section)))

; could-be : []int -> []int
(define (could-be nums)
  (fold (lambda (n r)
    (if (member n nums) r
      (cons n r))) '() '(1 2 3 4 5 6 7 8 9)))

; avaliable-at-position : [][]int int int -> []int
(define (avaliable-at-position board x y)
  (could-be (append
    (get-column board x)
    (get-row board y)
    (get-quad board (inexact->exact (floor (/ x 3)))
                    (inexact->exact (floor (/ y 3)))))))

; has-1-9? : []int -> bool
(define (has-1-9? nums)
  (let ((1-9 '(1 2 3 4 5 6 7 8 9)))
    (equal? 1-9 (fold (lambda (n r)
      (if (member n nums) (append r (list n)) r)) '() 1-9))))

; rows-solved? : [][]int -> bool
(define (rows-solved? board)
  (fold (lambda (row r)
    (and (has-1-9? row) r))
      #t board))

; cols-solved? : [][]int -> bool
(define (cols-solved? board)
  (fold (lambda (col r)
    (and (has-1-9? (get-column board col)) r))
      #t '(0 1 2 3 4 5 6 7 8)))

; quads-solved? : [][]int -> bool
(define (quads-solved? board)
  (fold (lambda (quad r)
    (and (has-1-9? (get-quad board (car quad) (cadr quad))) r))
      #t '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))

; is-solved? : [][]int -> bool
(define (is-solved? board)
  (and (rows-solved? board)
       (cols-solved? board)
       (quads-solved? board)))

; has-spaces? : [][]int -> bool
(define (has-spaces? board)
  (if (member 0 (fold append '() board)) #t #f))

; find-space [][]int -> []int{ x, y }
(define (find-space board)
  (fold (lambda (y r)
    (let ((x (fold (lambda (n r) (if (eq? (list-ref (list-ref board y) n) 0) n r)) #f '(0 1 2 3 4 5 6 7 8))))
      (if x (list x y) r))) #f '(0 1 2 3 4 5 6 7 8)))

; solve [][]int -> [][]int || #f
(define (solve board)
  (cond
    ((is-solved? board) board)
    ((not (has-spaces? board)) #f)
    ((has-spaces? board)
      (let ((pos (find-space board)))
        (fold (lambda (n r)
          (let ((test-board (set-position board (car pos) (cadr pos) n)))
            (or (solve test-board) r))) #f (avaliable-at-position board (car pos) (cadr pos)))))))
