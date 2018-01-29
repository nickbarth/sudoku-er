(use test)

(load "./sudoku.scm")

; test : set-row
(test "set-row"
  (cadr (set-row '((0 0 0) (0 0 0)) 1 '(5))) '(5))

; test : get-row
(let ((board (set-row '((0 0 0) (0 0 0)) 1 '(1 1 1))))
  (test "get-row"
    (get-row board 1) '(1 1 1)))

; test : set-position
(let ((board '((0 0 0) (0 0 0))))
  (test "set-position"
    (set-position board 1 1 1) '((0 0 0) (0 1 0))))

; test : get-column
(let ((board (set-row '((0 0 0)(0 0 0)(0 0 0)) 1 '(1 1 1))))
  (test "get-column"
    (get-column board 1) '(0 1 0)))

; test : get-quad
(let ((board '((1 2 3 0 0 0 0 0 0) (4 5 6 0 0 0 0 0 0 0 0 0) (7 8 9 0 0 0 0 0 0 0 0 0))))
  (test "get-quad"
    (get-quad board 0 0) '(1 2 3 4 5 6 7 8 9)))

; test : could-be
(test "could-be"
  (could-be '(1 2 3)) '(9 8 7 6 5 4))

; test : avaliable-at-position
(let ((board '((0 1 2 3 4 6 7 8 9) (0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0))))
  (test "available-at-position"
    (avaliable-at-position board 0 0) '(5)))

; test : has-1-9?
(test-assert "has-1-9"
  (has-1-9? '(2 1 3 4 5 6 7 8 9)))

; test : rows-solved?
(let ((board `((1 2 3 4 5 6 7 8 9)
               (1 2 3 4 5 6 7 8 9)
               (1 2 3 4 5 6 7 8 9)
               (1 2 3 4 5 6 7 8 9)
               (1 2 3 4 5 6 7 8 9)
               (1 2 3 4 5 6 7 8 9)
               (1 2 3 4 5 6 7 8 9)
               (1 2 3 4 5 6 7 8 9)
               (1 2 3 4 5 6 7 8 9))))
  (test-assert "rows-solved" (rows-solved? board)))

; test : cols-solved?
(let ((board `((1 1 1 1 1 1 1 1 1)
               (2 2 2 2 2 2 2 2 2)
               (3 3 3 3 3 3 3 3 3)
               (4 4 4 4 4 4 4 4 4)
               (5 5 5 5 5 5 5 5 5)
               (6 6 6 6 6 6 6 6 6)
               (7 7 7 7 7 7 7 7 7)
               (8 8 8 8 8 8 8 8 8)
               (9 9 9 9 9 9 9 9 9))))
  (test-assert "cols-solved" (cols-solved? board)))

; test : quads-solved?
(let ((board `((1 2 3 1 2 3 1 2 3)
               (4 5 6 4 5 6 4 5 6)
               (7 8 9 7 8 9 7 8 9)
               (1 2 3 1 2 3 1 2 3)
               (4 5 6 4 5 6 4 5 6)
               (7 8 9 7 8 9 7 8 9)
               (1 2 3 1 2 3 1 2 3)
               (4 5 6 4 5 6 4 5 6)
               (7 8 9 7 8 9 7 8 9))))
  (test-assert "quads-solved" (quads-solved? board)))

; test : has-spaces?
(let ((board `((1 1 1 1 1 1 1 1 1)
               (1 1 0 1 1 1 1 1 1))))
  (test-assert "has-spaces" (has-spaces? board)))

; test : find-space
(let ((board `((1 1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 1 1)
               (1 1 1 1 1 1 1 0 1)
               (1 1 1 1 1 1 1 1 1))))
  (test "find-space" (find-space board) '(7 7)))

(test-exit)
