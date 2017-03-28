import random

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
	random.seed()
	size = size + 1
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
