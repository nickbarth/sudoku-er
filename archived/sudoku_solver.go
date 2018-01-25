package main

type SudokuSolver struct {
	board [][]int
}

func NewSolver(board [][]int) *SudokuSolver {

	return &SudokuSolver{
		board: board,
	}
}

func Who() {
	fmt.Println("hello")
}
