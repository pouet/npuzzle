import random, os.path, argparse
from algo import *
from graphique import *
from time import sleep

def leave(message):
	print message
	exit()

def array_to_2d(array, size):
	arr = []
	for i in range(0, size):
		arr.append(array[i*size:i*size+size])
	return arr

def get_elem_pos_in_2d_array(array, element, size):
	for i in range(0, size):
		for j in range(0, size):
			if array[i][j] == element:
				return (i, j)
	return (-1, -1)

def get_row_empty_tile_2d(array, size):
	x, y = get_elem_pos_in_2d_array(array, 0, size)
	return x

def get_row_empty_tile(array, size):
	return get_row_empty_tile_2d(array_to_2d(array, size), size)

def get_free_places(array):
	arr = []
	for index, value in enumerate(array):
		if value == 0:
			arr.append(index)
	return arr

def get_pos_empty_tile_2d(array_2d, size):
	for i in range(0, size):
		for j in range(0, size):
			if array_2d[i][j] == 0:
				return (i, j)

def fill_top_right(array, x1, y1, x2, y2, nb, size):
	for i in range(x1, x2 + 1):
		if nb >= size * size:
			return array
		array[y1][i] = nb
		nb += 1
	
	for i in range(y1 + 1, y2 + 1):
		if nb >= size * size:
			return array
		array[i][x2] = nb
		nb += 1

	if x2 - x1:
		return fill_bottom_left(array, x1, y1 + 1, x2 - 1, y2, nb, size)
	return array

def fill_bottom_left(array, x1, y1, x2, y2, nb, size):
	for i in range(x2, x1 - 1, -1):
		if nb >= size * size:
			return array
		array[y2][i] = nb
		nb += 1
	
	for i in range(y2 - 1, y1 - 1, -1):
		if nb >= size * size:
			return array
		array[i][x1] = nb
		nb += 1

	if x2 - x1 > 0:
		return fill_top_right(array, x1 + 1, y1, x2, y2 - 1, nb, size)
	return array

def gen_snail(size):
	arr = [[0 for x in range(size)] for y in range(size)]
	return fill_top_right(arr, 0, 0, size - 1, size - 1, 1, size)
	
def get_empty_tile_row_from_size(size):
	return get_row_empty_tile_2d(gen_snail(size), size)

def check_top_right(array, x1, y1, x2, y2, element, nb, size, encountered):
	for i in range(x1, x2 + 1):
		if array[y1][i] == element:
			encountered = True
		elif array[y1][i] != 0 and array[y1][i] < element and encountered == True:
			nb += 1
	
	for i in range(y1 + 1, y2 + 1):
		if array[i][x2] == element:
			encountered = True
		elif array[i][x2] != 0 and array[i][x2] < element and encountered == True:
			nb += 1

	if x2 - x1:
		return check_bottom_left(array, x1, y1 + 1, x2 - 1, y2, element, nb, size, encountered)
	return nb

def check_bottom_left(array, x1, y1, x2, y2, element, nb, size, encountered):
	for i in range(x2, x1 - 1, -1):
		if array[y2][i] == element:
			encountered = True
		elif array[y2][i] != 0 and array[y2][i] < element and encountered == True:
			nb += 1
	
	for i in range(y2 - 1, y1 - 1, -1):
		if array[i][x1] == element:
			encountered = True
		elif array[i][x1] != 0 and array[i][x1] < element and encountered == True:
			nb += 1

	if x2 - x1 > 0:
		return check_top_right(array, x1 + 1, y1, x2, y2 - 1, element, nb, size, encountered)
	return nb

def check_snail_inversion(array_2d, size):
	count = 0
	for i in range(2, size * size):
		nb = check_top_right(array_2d, 0, 0, size - 1, size - 1, i, 0, size, False)
		count += nb
	return count

def check_inversion(array_1d, size):
	count = 0
	for i in range(0, size):
		if array_1d[i] != 0:
			for j in range(i + 1, size):
				if array_1d[j] != 0 and array_1d[i] > array_1d[j]:
					count += 1
	return count

def is_solvable(array, size):
	array_ref_2d = gen_snail(size)
	array_ref_1d = []
	for items in array_ref_2d:
		for item in items:
			array_ref_1d.append(item)

	count = check_snail_inversion(array_to_2d(array, size), size)

	if count % 2 == 0:
		return True
	return False

def is_valid(pt, mv, size):
	x, y = pt
	a, b = mv
	x = x + a
	y = y + b
	return x >= 0 and x < size and y >= 0 and y < size

def print_puzzle(puzzle, size):
	nb_spaces = len(str(size * size - 1))

	print 'Puzzle :'
	for idx, value in enumerate(puzzle):
		if idx % size == 0 and idx != 0:
			print ''
		print value,
		nb = nb_spaces - len(str(value))
		print ' ' * nb,
	print
	print

def gen_puzzle(size, solvable):
	size_total = size * size
	grid = gen_snail(size)
	if solvable == False:
		y_last, x_last = get_elem_pos_in_2d_array(grid, size_total - 1, size)
		y_before, x_before = get_elem_pos_in_2d_array(grid, size_total - 2, size)
		tmp = grid[y_last][x_last]
		grid[y_last][x_last] = grid[y_before][x_before]
		grid[y_before][x_before] = tmp
	moves = [ (-1, 0), (1, 0), (0, -1), (0, 1) ]
	empty = get_pos_empty_tile_2d(grid, size)

	for i in range(0, 1000):
		r = random.randint(0, 3)
		if is_valid(empty, moves[r], size):
			new = (empty[0] + moves[r][0], empty[1] + moves[r][1])
			tmp = grid[empty[0]][empty[1]]
			grid[empty[0]][empty[1]] = grid[new[0]][new[1]]
			grid[new[0]][new[1]] = tmp
			empty = new
	arr = []
	for items in grid:
		for item in items:
			arr.append(item)
	return arr

def check_line_size(line, size):
	split = filter(None, line.split(" "))
	if len(split) != size:
		leave('Line with the wrong format in file.')

	'''Filter only numbers - Allow us to see unwanted characters.'''
	if len([item for item in split if item.isdigit()]) != size:
		leave('Line with the wrong format in file.')
	return split

def add_line_to_puzzle(line, puzzle, size):
	for item in [int(item) for item in split]:
		if item in puzzle:
			leave('Same number appears multiple times.')
		elif item < 0 or item >= size*size:
			leave('Number out of range of the given size.')
		puzzle.append(item)
	return puzzle

def remove_useless_char_from_line(line):
	pos = line.find('#')
	if pos != -1:
		line = line[:pos]
	line = line.strip()
	return line

if __name__ == "__main__":
	parser = argparse.ArgumentParser()

	size = -1
	parser.add_argument("-f", "--file", type=str, help="The file from which we read the puzzle.")
	parser.add_argument("-s", "--size", type=int, help="Choose the size of the randomly generated puzzle. Size must be == 3 (Not used if a filename is specified).")
	parser.add_argument("--solvable", action="store_true", default=False, help="Create a solvable puzzle.")
	parser.add_argument("--unsolvable", action="store_true", default=False, help="Create an unsolvable puzzle.")
	parser.add_argument("--manhattan", "--m",  action="store_true", default=False, help="Use manhattan distance for heuristic function.")
	parser.add_argument("--linearconflict", "--lc", action="store_true", default=False, help="Use linear conflict as heuristic function.")
	parser.add_argument("--euclidean","--e", action="store_true", default=False, help="Use euclidean distance for heuristic function.")
	parser.add_argument("-g", "--graphics", action="store_true", default=False, help="Create a graphic version of the solution.")
#	parser.add_argument("-o", "--ocaml", action="store_true", default=False, help="Send the puzzle in stdout in ocaml format")

	args = parser.parse_args()
	heuristic = 'm'
	puzzle = []
	solvable = random.choice([True, False])
	if args.solvable and args.unsolvable:
		leave('Can\'t choose --solvable and --unsolvable together.')
	elif args.solvable:
		solvable = True
	elif args.unsolvable:
		solvable = False

	'''Traitement sur les fonctions heuristics'''
	choice_h = 0
	if not args.manhattan and not args.linearconflict and not args.euclidean:
		leave('You must choose at least one heuristic function.')
	elif (args.manhattan and args.linearconflict) or (args.manhattan and args.euclidean) or (args.linearconflict and args.euclidean):
		leave('You must choose only one heuristic function.')
	elif args.manhattan:
		choice_h = 0
	elif args.linearconflict:
		choice_h = 1
        elif args.euclidean:
            choice_h = 2

	if args.file == None and args.size == None:
		leave('Must specify a filename or at least a size to generate a random puzzle.')
	elif args.file == None and args.size != None:
		if args.size != 3:
			leave('Invalid size for the random generated puzzle.')
		size = args.size
		puzzle = gen_puzzle(size, solvable)
	else:
		if os.path.isfile(args.file) == False:
			leave('Invalid filename in argument.')
		with open(args.file, 'r') as f:
			for line in f:
				line = remove_useless_char_from_line(line)

				if len(line) > 0:
					if size == -1 and line.isdigit() == False:
						leave('Missing size of the puzzle.')
					elif size == -1 and line.isdigit() == True:
						size = int(line)
						if size != 3:
							leave('Invalid size (must be equal to 3).')
					elif size != -1:
						split = check_line_size(line, size)
						'''Means we have reached the maximum number of line and we want to add another'''
						if len(puzzle) == size*size:
							leave('Too many lines for the puzzle.')
						puzzle = add_line_to_puzzle(split, puzzle, size)

		if size == -1 or (size != -1 and len(puzzle) != size*size):
			leave('Missing informations in file.')
	
	'''	if args.ocaml:
		string = ''
		for item in puzzle:
			string = string + str(item)
		print "[| " + "; ".join(string)+ "; |]"
	else:'''
	print_puzzle(puzzle, size)
	if is_solvable(puzzle, size) == False:
		leave('This puzzle can\'t be solved.')
	res = py_algo(puzzle, choice_h)
	'''Gather the solution from the function solve and set it in the constructor
	@TODO'''
	if args.graphics:
		game = Game(puzzle, size, res)
		#game = Game([6, 12, 5, 11, 13, 8, 1, 7, 9, 2, 3, 10, 14, 4, 15, 0], 4, res)
