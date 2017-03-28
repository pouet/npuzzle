import sys, random, os.path
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

def gen_puzzle(size):
	size = size + 1
	arr = [0] * size
	for i in range(1, size):
		arr[random.choice(get_free_places(arr))] = i
	if is_solvable(arr) != True:
		length = len(arr)
		for index, value in enumerate(arr):
			if index < length - 1 and arr[index] != 0 and arr[index + 1] != 0:
				tmp = arr[index]
				arr[index] = arr[index + 1]
				arr[index + 1] = tmp
				break
	return arr

array = []
size = 0
length_args = len(sys.argv)
if length_args > 2:
	leave('Too many arguments')
elif length_args == 1:
	size = random.randint(3, 15)
	array = gen_puzzle(size)
else:
	'''Filename in arguments'''
	if os.path.isfile(sys.argv[1]) == False:
		leave('Invalid filename in argument.')
#penis.solve(size, array, array.index(0))
