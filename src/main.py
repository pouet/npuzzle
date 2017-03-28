import random, os.path, argparse
#import penis

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

if __name__ == "__main__":
	parser = argparse.ArgumentParser()

	size = -1
	parser.add_argument("-f", "--file", type=str, help="The file from which we read the puzzle.")
	parser.add_argument("-s", "--size", type=int, help="Choose the size of the randomly generated puzzle. (Not used if a filename is specified).")

	args = parser.parse_args()
	array = []

	if args.file == None and args.size == None:
		leave('Must specify a filename or at least a size to generate a random puzzle.')
	elif args.file == None and args.size != None:
		size = args.size
		array = gen_puzzle(size, random.choice([True, False]))
		nb_spaces = len(str(size * size - 1))
		
		print 'Puzzle generated :'
		for idx, value in enumerate(array):
			if idx % size == 0 and idx != 0:
				print ''
			print value,
			nb = nb_spaces - len(str(value))
			print ' ' * nb,
		print
		print
	else:
		if os.path.isfile(args.file) == False:
			leave('Invalid filename in argument.')
		with open(args.file, 'r') as f:
			for line in f:
				pos = line.find('#')
				if pos != -1:
					line = line[:pos]
				line = line.strip()
	
				if len(line) > 0:
					if size == -1 and line.isdigit() == False:
						leave('Missing size of the puzzle.')
					elif size == -1 and line.isdigit() == True:
						size = int(line)
						if size <= 0:
							leave('Invalid size.')
					elif size != -1:
						'''Remove empty elements'''
						split = filter(None, line.split(" "))
						if len(split) != size:
							leave('Line with the wrong format in file.')
	
						'''Filter only numbers - Allow us to see unwanted characters.'''
						if len([item for item in split if item.isdigit()]) != size:
							leave('Line with the wrong format in file.')

						'''Means we have reached the maximum number of line and we want to add another'''
						if len(array) == size*size:
							leave('Too many lines for the puzzle.')
	
						for item in [int(item) for item in split]:
							if item in array:
								leave('Same number appears multiple times.')
							elif item < 0 or item >= size*size:
								leave('Number out of range of the given size.')
							array.append(item)
	
		if size == -1 or (size != -1 and len(array) != size*size):
			leave('Missing informations in file.')
	if is_solvable(array) == False:
		leave('This puzzle can\'t be solved.')
	#penis.solve(size, array, array.index(0))
