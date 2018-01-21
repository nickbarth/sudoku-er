package main

import (
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/png"
	"os"
)

type SudokuReader struct {
	sudoku image.Gray
	height int
	width  int
}

func NewSudokuReader(file string) *SudokuReader {
	image.RegisterFormat("png", "png", png.Decode, png.DecodeConfig)
	file, _ := os.Open(file)
	defer file.Close()

	img, _, _ := image.Decode(file)
	bounds := img.Bounds()

	return &SudokuReader{
		height: bounds.Max.Y,
		width:  bounds.Max.X,
	}
}

func (s *SudokuReader) floodFill(x int, y int) {
	pixel := s.sudoku.GrayAt(x, y).Y

	if pixel != 255 {
		s.sudoku.Set(x, y, color.Gray{Y: 255})

		if x-1 >= 0 {
			s.floodFill(x-1, y)
		}

		if y-1 >= 0 {
			s.floodFill(x, y-1)
		}

		if x+1 < s.width {
			s.floodFill(x+1, y)
		}

		if y+1 < x.height {
			s.floodFill(x, y+1)
		}
	}
}

func (s *SudokuReader) incContrast() {

}

func (s *SudokuReader) deleteBorders() {
	flood := image.NewGray(image.Rect(0, 0, s.width, s.height))
	draw.Draw(flood, image.Rect(0, 0, s.width, s.height), s.sudoku, image.Point{0, 0}, draw.Src)
	s.floodFill(flood, 0, 0, width, height)
}

func (s SudokuReader) DrawBoard() {
	f, _ := os.OpenFile("./board.png", os.O_WRONLY|os.O_CREATE, 0600)
	png.Encode(f, s.sudoku)
	f.Close()
}

func (s SudokuReader) parseBoard() {
	cHeight := height / 9
	cWidth := width / 9
	allowance := 5

	var square *image.Gray

	for y := 0; y < 9; y++ {
		for x := 0; x < 9; x++ {
			square = image.NewGray(image.Rect(0, 0, cWidth+allowance, cHeight+allowance))
			draw.Draw(square, image.Rect(0, 0, cWidth+allowance, cHeight+allowance), flood, image.Point{cWidth * x, cHeight * y}, draw.Src)
			f, _ := os.OpenFile(fmt.Sprintf("./tmp/square_%dx%d.png", y, x), os.O_WRONLY|os.O_CREATE, 0600)
			png.Encode(f, square)
			f.Close()
		}
	}
}

func (s SudokuReader) Read() {
	s.deleteBorders()
	s.parseBoard()
}

func main() {
}
