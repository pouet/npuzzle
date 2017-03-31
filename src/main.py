import random, os.path, argparse
#import penis
from graphique import *
from time import sleep

def leave(message):
	print message
	exit()

def get_free_places(array):
	arr = []
	for index, value in enumerate(array):
		if value == 0:
			arr.append(index)
	return arr

def is_solvable(array):
	count = 0
	length = len(array)
	for i in array:
		if array[i] != 0:
			for j in range(i + 1, length):
				if array[j] < array[i]:
					count += 1
	if count % 2 == 0:
		return True
	return False

def gen_puzzle(size, solvable):
	size *= size
	arr = [0] * size
	for i in range(1, size):
		arr[random.choice(get_free_places(arr))] = i
	if is_solvable(arr) != solvable:
		length = len(arr)
		for index, value in enumerate(arr):
			if index < length - 1 and arr[index] != 0 and arr[index + 1] != 0:
				tmp = arr[index]
				arr[index] = arr[index + 1]
				arr[index + 1] = tmp
				break
	return arr

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
	parser.add_argument("-s", "--size", type=int, help="Choose the size of the randomly generated puzzle. Size must be > 3 and <= 7 (Not used if a filename is specified).")
	parser.add_argument("--solvable", action="store_true", default=False, help="Create a solvable puzzle.")
	parser.add_argument("--unsolvable", action="store_true", default=False, help="Create an unsolvable puzzle.")
	parser.add_argument("-g", "--graphics", action="store_true", default=False, help="Create a graphic version of the solution.")
	parser.add_argument("--ocaml", action="store_true", default=False, help="Send the puzzle in stdout in ocaml format")

	args = parser.parse_args()
	puzzle = []
	solvable = random.choice([True, False])
	if args.solvable and args.unsolvable:
		leave('Can\'t choose --solvable and --unsolvable together.')
	elif args.solvable:
		solvable = True
	elif args.unsolvable:
		solvable = False

	if args.file == None and args.size == None:
		leave('Must specify a filename or at least a size to generate a random puzzle.')
	elif args.file == None and args.size != None:
		if args.size < 3 or args.size > 7:
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
						if size <= 2:
							leave('Invalid size.')
					elif size != -1:
						split = check_line_size(line, size)
						'''Means we have reached the maximum number of line and we want to add another'''
						if len(puzzle) == size*size:
							leave('Too many lines for the puzzle.')
						puzzle = add_line_to_puzzle(split, puzzle, size)

		if size == -1 or (size != -1 and len(puzzle) != size*size):
			leave('Missing informations in file.')
	
	if args.ocaml:
		string = ''
		for item in puzzle:
			string = string + str(item)
		print "[|" + "; ".join(string)+ "|]"
	else:
		print_puzzle(puzzle, size)
	if is_solvable(puzzle) == False:
		leave('This puzzle can\'t be solved.')
	#penis.solve(size, puzzle, puzzle.index(0))
	'''Gather the solution from the function solve and set it in the constructor
	@TODO'''
	#if args.graphics:
		#game = Game(puzzle, size)
		#game = Game([6, 12, 5, 11, 13, 8, 1, 7, 9, 2, 3, 10, 14, 4, 15, 0], 4, 'BDDBBGHHDHGBDDBBGGHDDBGGGHHDBBGHHHDBGHDDDBGGBBGHHH')
