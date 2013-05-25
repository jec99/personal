import math

def prime(n):
	if n == 2 | n == 3:
		return True
	elif n < 2 | n % 2 == 0 | n % 3 == 0:
		return False
	else:
		for x in xrange(6, int(math.floor(math.sqrt(n))), 6):
			if n % (x - 1) == 0:
				return False
			elif n % (x + 1) == 0:
				return False
	return True

print prime(2)