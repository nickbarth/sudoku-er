default:
	csc -c sudoku.scm
	csc sudoku-er.scm sudoku.o -o sudoku-er

test:
	csi -ss sudoku_test.scm

clean:
	rm -f sudoku.o sudoku-er
