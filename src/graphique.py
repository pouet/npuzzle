from Tkinter import *

TAILLE_TILE = 40

def array_to_2d(array, size):
	arr = []
	for i in range(0, size):
		arr.append(array[i*size:i*size+size])
	return arr

class Game:
	def __init__(self, puzzle, size):
		self.puzzle_2d = array_to_2d(puzzle, size)
		self.size = size
		self.texts_canvas = []
		self.fenetre = Tk()
		self.fenetre.title("N-Puzzle")
		size_int = size * TAILLE_TILE
		self.fenetre.resizable(0, 0)
		self.fenetre.geometry(str(size_int) + 'x' + str(size_int))
	
		self.canevas = Canvas(self.fenetre, width=size_int, height=size_int, bg='white', bd=0, highlightthickness=0)
		for i in range(1, size):
			self.canevas.create_line(i * TAILLE_TILE,  0, i * TAILLE_TILE, size_int)
			self.canevas.create_line(0, i * TAILLE_TILE , size_int, i * TAILLE_TILE)
		for i in range(0, size):
			tmp = []
			for j in range(0, size):
				text = ''
				if self.puzzle_2d[i][j] != 0:
					text = str(self.puzzle_2d[i][j])
				tmp.append(self.canevas.create_text(((j + 1) * TAILLE_TILE) - (TAILLE_TILE / 2), ((i + 1) * TAILLE_TILE) - (TAILLE_TILE / 2), text=text, width=TAILLE_TILE/2))
			self.texts_canvas.append(tmp)
		self.fenetre.bind('<Left>', self.left_move)
		self.fenetre.bind('<Right>', self.right_move)
		self.fenetre.bind('<Up>', self.up_move)
		self.fenetre.bind('<Down>', self.down_move)
		self.fenetre.bind('q', self.quit)
		self.canevas.pack(anchor='center', fill=BOTH, expand=1)
		self.fenetre.mainloop()
		self.fenetre.destroy()
	
	def get_coords_empty_tile(self):
		for i in range(0, self.size):
			for j in range(0, self.size):
				if self.puzzle_2d[i][j] == 0:
					return (i, j)
		return (None, None)

	def quit(self, event):
		self.fenetre.quit()
	
	def swap_tiles(self, x0, y0, x1, y1, tab):
		if tab == 'P':
			tmp = self.puzzle_2d[x0][y0]
			self.puzzle_2d[x0][y0] = self.puzzle_2d[x1][y1]	
			self.puzzle_2d[x1][y1] = tmp
		elif tab == 'T':
			tmp = self.texts_canvas[x0][y0]
			self.texts_canvas[x0][y0] = self.texts_canvas[x1][y1]	
			self.texts_canvas[x1][y1] = tmp

	def move_tile(self, x, y, moveX, moveY):
		for i in range(0, TAILLE_TILE):
			self.canevas.move(self.texts_canvas[x][y], moveX, moveY)

	def left_move(self, event):
		x, y = self.get_coords_empty_tile()
		if y < self.size - 1 :
			self.swap_tiles(x, y, x, y + 1, 'P')
			self.move_tile(x, y + 1, -1, 0)
			self.swap_tiles(x, y, x, y + 1, 'T')

	def right_move(self, event):
		x, y = self.get_coords_empty_tile()
		if y > 0 :
			self.swap_tiles(x, y, x, y - 1, 'P')
			self.move_tile(x, y - 1, 1, 0)
			self.swap_tiles(x, y, x, y - 1, 'T')

	def up_move(self, event):
		x, y = self.get_coords_empty_tile()
		if x < self.size - 1 :
			self.swap_tiles(x, y, x + 1, y, 'P')
			self.move_tile(x + 1, y, 0, -1)
			self.swap_tiles(x, y, x + 1, y, 'T')

	def down_move(self, event):
		x, y = self.get_coords_empty_tile()
		if x > 0 :
			self.swap_tiles(x, y, x - 1, y, 'P')
			self.move_tile(x - 1, y, 0, 1)
			self.swap_tiles(x, y, x - 1, y, 'T')
