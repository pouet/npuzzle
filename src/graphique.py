from Tkinter import *

TAILLE_TILE = 40
texts_canvas = []
def array_to_2d(array, size):
	arr = []
	for i in range(0, size):
		arr.append(array[i*size:i*size+size])
	return arr

def main(size, puzzle):
	puzzle_2d = array_to_2d(puzzle, size)
	fenetre = Tk()
	fenetre.title("N-Puzzle")
	size_int = size * TAILLE_TILE
	size_str = str(size_int)
	fenetre.resizable(0, 0)
	fenetre.geometry(size_str + 'x' + size_str)
	fenetre.grid()

	canevas = Canvas(fenetre, width=size_int, height=size_int, bg='white', bd=0, highlightthickness=0)
	for i in range(1, size_int / TAILLE_TILE):
		canevas.create_line(i * TAILLE_TILE,  0, i * TAILLE_TILE, size_int)
		canevas.create_line(0, i * TAILLE_TILE , size_int, i * TAILLE_TILE)
	for i in range(0, size):
		tmp = []
		for j in range(0, size):
			if puzzle_2d[i][j] != 0:
				tmp.append(canevas.create_text(((j + 1) * TAILLE_TILE) - (TAILLE_TILE / 2), ((i + 1) * TAILLE_TILE) - (TAILLE_TILE / 2), text=str(puzzle_2d[i][j]), width=TAILLE_TILE/2))
		texts_canvas.append(tmp)
	canevas.pack(anchor='center', fill=BOTH, expand=1)
	fenetre.mainloop()
