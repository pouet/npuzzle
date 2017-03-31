import random

def is_valid(pt, mv):
	x, y = pt
	a, b = mv
	x = x + a
	y = y + b
	return x >= 0 and x < 3 and y >= 0 and y < 3

def gen_puzzle():
	grid = [ [ 1, 2, 3 ], [ 8, 0, 4 ], [ 7, 6, 5 ] ]
	moves = [ (-1, 0), (1, 0), (0, -1), (0, 1) ]
	empty = (1, 1)

	for i in range(0, 1000):
		r = random.randint(0, 3)
		if is_valid(empty, moves[r]):
			new = (empty[0] + moves[r][0], empty[1] + moves[r][1])
			tmp = grid[empty[0]][empty[1]]
			grid[empty[0]][empty[1]] = grid[new[0]][new[1]]
			grid[new[0]][new[1]] = tmp
			empty = new

	s = ''
	for it in grid:
		for j in it:
			s = s + str(j) + '; '
	print 'let grid = [| ' + s + '|] in'

gen_puzzle()
