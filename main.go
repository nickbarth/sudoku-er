package main

import (
	"fmt"
	"image"
	"image/color"
	"image/draw"
	"image/png"
	"os"
)

func bFill(img *image.Gray, x int, y int, mX int, mY int) {
	c := img.GrayAt(x, y).Y

	if c != 255 {
		img.Set(x, y, color.Gray{Y: 255})

		if x-1 >= 0 {
			bFill(img, x-1, y, mX, mY)
		}

		if y-1 >= 0 {
			bFill(img, x, y-1, mX, mY)
		}

		if x+1 < mX {
			bFill(img, x+1, y, mX, mY)
		}

		if y+1 < mY {
			bFill(img, x, y+1, mX, mY)
		}
	}
}

func main() {
	image.RegisterFormat("png", "png", png.Decode, png.DecodeConfig)
	file, _ := os.Open("./tiny.png")
	img, _, _ := image.Decode(file)
	file.Close()

	bounds := img.Bounds()
	width, height := bounds.Max.X, bounds.Max.Y

	fmt.Println(width, height)

	flood := image.NewGray(image.Rect(0, 0, width, height))
	draw.Draw(flood, image.Rect(0, 0, width, height), img, image.Point{0, 0}, draw.Src)
	bFill(flood, 0, 0, width, height)

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

	square = image.NewGray(image.Rect(0, 0, width, height))
	draw.Draw(square, image.Rect(0, 0, width, height), flood, image.Point{0, 0}, draw.Src)
	f, _ := os.OpenFile("./tmp/full.png", os.O_WRONLY|os.O_CREATE, 0600)
	png.Encode(f, square)
	f.Close()
}
