def factor(n):
	factors = []
	x = n

	while x != 1:
		for i in xrange(2, x + 1):
			if x % i == 0:
				factors.append(i)
				x = x / i
				continue

	return factors

print factor(317584931803)