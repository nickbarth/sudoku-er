(import chicken scheme)
(use srfi-13)
(use utils)

(declare (uses sudoku))

; pp : prints board
(define (pp board)
  (for-each (lambda (row)
    (for-each (lambda (n)
      (display n) (display " ")) row) (print)) board))

; make-board : []int -> [][]int
(define (make-board board)
  (if (pair? board)
    (cons (take board 9) (make-board (drop board 9))) '() ))

; main : argv read file
(let ((board (make-board (map string->number
                         (map string (string->list
                         (string-delete #\newline
                         (string-delete #\space
                         (read-all (cadr (argv)))))))))))
  (pp board)
  (print "-----")
  (pp (solve board)))
