# format: list( list(money left, vector(coins so far)) )

dens <- c(1, 2, 5, 10, 20, 50, 100, 200)

combs.recursive <- function(ls) {
	l1 <- ls[[1]]  # first part
	l <- ls
	l[[1]] <- NULL  # rest of list
	if (l1[[1]] == 0) {
		if (length(ls) == 1) {
			return(l1)
		} else {
			return(c(l1, combs(l)))
		}
	} else {
		subs <- dens[dens <= l1[[1]]]
		l2 <- list(list())
		for (i in subs) {
			l2 <- c(l2, list(list(l1[[1]] - i, c(l1[[2]], i))))
		}
		l2[[1]] <- NULL
		return(combs(c(l2, l)))
	}
}

combinations.recursive <- function(n) {
	c <- combs(list(list(n, vector(length=0))))
	c <- unique(lapply(c, sort))
	c[[1]] <- NULL
	c
}

# format: list( list( element, vector(things added to get to that element) ) )
combs <- function(n) {
	l <- list(list(n, vector(length=0)))
	while (!allfirstzeroes(l)) {
		tagged <- vector(length=0)
		len <- length(l)
		for (i in 1:len) {
			vec <- l[[i]]
			if (vec[[1]] != 0) {
				subs <- dens[dens <= vec[[1]]]
				for (s in subs) {
					l <- append(l, list(list(vec[[1]] - s, c(vec[[2]], s))))
				}
				tagged <- c(tagged, i)
			}
		}
		l[tagged] <- NULL
	}
	l
}

allfirstzeroes <- function(l) {
	all(sapply(l, function(x) { return(x[[1]]) }) == 0)
}

combinations <- function(n) {
	unique(lapply(lapply(combs(n), function(x) { return(x[[2]]) }), sort))
}

done <- vector(length=0)

combines <- function(n) {
	done <- vector(length=0)
	l <- list()
	if (n <= 10) {
		return(combinations(n))
	} else {
		subs <- dens[dens <= n]
		for (i in subs) {
			l <- append(l, lapply(combines(n - i), function(x) { c(x, i) }))
		}
	}
	return(l)
}