pythagorean <- function(n) {
	for (a in 1:n) {
		for (b in 1:n) {
			c <- n - a - b
			if (a^2 + b^2 == c^2) return(list(a, b, c))
		}
	}
}

# all primes below a certain number
primes <- function(n) {
	nums <- c(2, seq(3, n, 2))
	primes <- vector(length=0)
	while (length(nums) != 0) {
		primes <- c(primes, nums[1])
		nums <- nums[nums %% nums[1] != 0]
	}
	return(primes)
}

primes.2 <- function(n) {
	primes <- 5
	for (i in 5:n) {
		if (is.prime(i)) primes <- primes + i
	}
	return(primes)
}

primes.3 <- function(n) {
	primes <- 5
	for (i in seq(6, n, 6)) {
		if (is.prime(i - 1)) primes <- primes + i - 1
		if (is.prime(i + 1)) primes <- primes + i + 1
	}
	return(primes)
}

is.prime <- function(n) {
	for (i in primeslist) {
		if (n == i) {
			return(TRUE)
		} else if (n %% i == 0) {
			return(FALSE)
		}
	}
	if (n < 99990 * 99990) {
		return(TRUE)
	}
	for (i in seq(99990, floor(sqrt(n)), 6)) {
		if (n %% (i + 1) == 0 || n %% (i - 1) == 0) {
			return(FALSE)
		}
	}
	return(TRUE)
}