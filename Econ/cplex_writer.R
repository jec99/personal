# these three are deprecated and only useful for simple testing
make_cplex_file_2 <- function(u.before, u.after, pops) {
	diffs <- u.before - u.after
	sqrts <- sqrt(pops)
	sqrtsum <- sqrt(sum(pops))

	mns <- "Minimize\n  eps1"
	cons1 <- "Subject To\n"
	cons2 <- ""
	for (i in 1:length(pops)) {
		cons1 <- paste(cons1, sprintf("  s%d%d + s%d%d + s%d%d + s%d%d == 1\n", i, 1, i, 2, i, 3, i, 4), sep="")
		cons2 <- paste(cons2, sprintf("  eps%d => %f\n", i, diffs[i]), sep="")
		if (i != 1)
			mns <- paste(mns, sprintf(" + eps%d", i), sep="")
	}
	mns <- paste(mns, "\n", sep="")

	cons3 <- ""
	for (i in 1:length(pops)) {
		cons3tmp <- sprintf("  -eps%d + [", i)
		for (j in 1:length(pops))
			for (k in 1:4)
				cons3tmp <- paste(cons3tmp, sprintf("%f s%d%d * s%d%d + ", pops[j] / sqrtsum, i, k, j, k), sep="")

		cons3tmp <- substr(cons3tmp, 1, nchar(cons3tmp) - 3)
		for (k in 1:4)
			cons3tmp <- paste(cons3tmp, sprintf(" - %f s%d%d^2", sqrts[i], i, k), sep="")

		cons3t <- paste(cons3tmp, "] == 0\n", sep="")
		cons3 <- paste(cons3, cons3t, sep="\n")
	}

	bds <- "Bounds\n"
	cons4 <- ""
	for (i in 1:length(pops))
		for (k in 1:4)
			cons4 <- paste(cons4, sprintf("  0 <= s%d%d\n", i, k))

	bds <- paste(bds, cons4, sep="")
	cons <- paste(cons1, cons2, cons3, sep="")
	out <- paste(mns, cons, bds, "End", sep="")
	return(out)
}

make_cplex_file <- function(u.before, u.after, pops) {
	diffs <- u.before - u.after
	sqrts <- sqrt(pops)
	sqrtsum <- sqrt(sum(pops))

	mns <- "Minimize\n  eps1"
	cons1 <- "Subject To\n"
	cons2 <- ""
	for (i in 1:length(pops)) {
		cons1 <- paste(cons1, sprintf("  s%d%d + s%d%d + s%d%d + s%d%d == 1\n", i, 1, i, 2, i, 3, i, 4), sep="")
		cons2 <- paste(cons2, sprintf("  eps%d => %f\n", i, diffs[i]), sep="")
		if (i != 1)
			mns <- paste(mns, sprintf(" + eps%d", i), sep="")
	}
	mns <- paste(mns, "\n", sep="")

	cons3 <- ""
	for (i in 1:length(pops)) {
		cons3tmp <- sprintf("  -eps%d + [", i)
		
		for (k in 1:4)
			cons3tmp <- paste(cons3tmp, sprintf("s%d%d * wmean%d + ", i, k, k), sep="")
		cons3tmp <- substr(cons3tmp, 1, nchar(cons3tmp) - 3)
		for (k in 1:4)
			cons3tmp <- paste(cons3tmp, sprintf(" - %f s%d%d^2", sqrts[i], i, k), sep="")

		cons3tmp <- paste(cons3tmp, "] == 0\n", sep="")
		cons3 <- paste(cons3, cons3tmp, sep="\n")
	}
	cons3 <- paste(cons3, "\n", sep="")

	cons3.5 <- ""
	for (i in 1:4) {
		cons3.5t <- sprintf("  -%f wmean%d", sqrtsum, i)
		for (j in 1:length(pops))
			cons3.5t <- paste(cons3.5t, sprintf(" + %f s%d%d", pops[j], j, i), sep="")
		cons3.5t <- paste(cons3.5t, " == 0\n", sep="")
		cons3.5 <- paste(cons3.5, cons3.5t, sep="")
	}

	bds <- "Bounds\n"
	cons4 <- ""
	for (i in 1:length(pops))
		for (k in 1:4)
			cons4 <- paste(cons4, sprintf("  0 <= s%d%d\n", i, k), sep="")

	bds <- paste(bds, cons4, sep="")
	cons <- paste(cons1, cons2, cons3, cons3.5, sep="")
	out <- paste(mns, cons, bds, "End", sep="")
	return(out)
}

write.cplex.file <- function(u.before, u.after, pops, filename, d=getwd()) {
	stopifnot(length(u.before) == length(pops) && length(u.after) == length(pops))
	o <- make_cplex_file(u.before, u.after, pops)
	cat(o, file=paste(d, "/", filename, sep=""))
	return(o)
}

# l is a list of all possible mergers represented as vectors with utilities as
# entries and names of prefectures as names. fin.merge is a convenience duplicate
# of the merger containing all of the municipalities; this should also be in l.
# pops is a named vector with the populations of the municipalities. lobound is
# the lower bound on alpha; using -inf takes a long time for the program to run;
# a value of about -1000 is suggested. the writer functions both default to
# saving in the working directory, but a directory can be supplied

make_cplex_file_interference <- function(l, fin.merge, pops) {
	# data format: list containing vectors, and each vector is a merger whose names are those
	# of the involved parties and whose entries are their utilities in that merger.

	# fin.merge is the final merger, for convenience. it is also contained inside the list
	# pops is the named populations vector

	# names may not contain illegal characters in the cplex lp format.

	precs <- unique(names(fin.merge))

	final <- paste("_", paste(precs, collapse="."), sep="")
	mns <- "Minimize\n\t"
	cons1 <- "Subject To\n"
	for (i in precs) {
		cons1 <- paste(cons1, sprintf("\ts%s$%d + s%s$%d + s%s$%d + s%s$%d == 1\n",
			i, 1, i, 2, i, 3, i, 4), sep="")
		mns <- paste(mns, "eps", i, final, " + ", sep="")
	}
	mns <- paste(substr(mns, 1, nchar(mns) - 3), "\n", sep="")
	cons1 <- substr(cons1, 1, nchar(cons1) - 1)

	cons3 <- "\t"
	cons3.5 <- "\t"
	for (i in l) {
		nms <- names(i)
		cons3cons <- ""
		cons3sum <- "\n\t"
		cons3.5cons <- ""
		cur <- paste("_", paste(nms, collapse="."), sep="")

		# make the secession variables, constraints
		# do not make a secession variable for the final merger
		if (length(i) != length(fin.merge)) {
			for (j in nms) {
				cons3tmp <- paste("secede", j, cur, " = 1 -> eps", j, final, " => ",
					i[[j]], " - ", fin.merge[[j]], " + eps", j, cur, sep="")
				cons3cons <- paste(cons3cons, cons3tmp, sep="\n\t")
				cons3sum <- paste(cons3sum, "secede", j, cur, " + ", sep="")
			}
			cons3sum <- paste(substr(cons3sum, 1, nchar(cons3sum) - 3), " => 1", sep="")
			cons3 <- paste(cons3, paste(cons3sum, cons3cons, sep="\t"), sep="\n")
		}

		# make epsilons
		for (j in nms) {
			cons3.5tmp <- paste("-eps", j, cur, " + [", sep="")
			sqs <- as.numeric(sqrt(sum(pops[i])))
			for (k in 1:4)
				for (m in nms)
					cons3.5tmp <- paste(cons3.5tmp, sprintf("%f s%s$%d * s%s$%d + ",
						pops[[m]] / sqs, j, k, m, k), sep="")

			cons3.5tmp <- paste(substr(cons3.5tmp, 1, nchar(cons3.5tmp) - 3), "] == 0\n", sep="")
			cons3.5cons <- paste(cons3.5cons, cons3.5tmp, sep="\n\t")
		}
		cons3.5 <- paste(cons3.5, cons3.5cons, sep="")
	}

	cons4 <- "Bounds\n"
	for (i in precs)
		for (k in 1:4)
			cons4 <- paste(cons4, sprintf("\t0 <= s%s$%d\n", i, k))

	cons5 <- "Binary\n\t"
	for (i in l)
		if (length(i) != length(fin.merge))
			for (j in names(i))
				cons5 <- paste(cons5, "secede", j, "_", paste(names(i), collapse="."), "\n\t", sep="")

	bds <- paste(cons4, cons5, sep="\n")
	cons <- paste(cons1, paste(cons3, cons3.5, sep="\n"), sep="")
	out <- paste(mns, cons, bds, "End", sep="\n")
	return(out)
}

make_cplex_file_interference_alpha <- function(l, fin.merge, pops, lobound) {
	# data format: list containing vectors, and each vector is a merger whose names are thos
	# of the involved parties and whose entries are their utilities in that merger.

	precs <- unique(names(fin.merge))

	final <- paste("_", paste(precs, collapse="."), sep="")
	mns <- "Minimize\n\t"
	cons1 <- "Subject To\n"
	for (i in precs) {
		cons1 <- paste(cons1, sprintf("\ts%s$%d + s%s$%d + s%s$%d + s%s$%d == 1\n",
			i, 1, i, 2, i, 3, i, 4), sep="")
		mns <- paste(mns, "eps", i, final, " + ", sep="")
	}
	mns <- paste(mns, "10000000 isfeasible", "\n", sep="")
	cons1 <- substr(cons1, 1, nchar(cons1) - 1)

	cons2 <- "\t"
	for (p in precs)
		for (k in 1:4)
			cons2 <- paste(cons2, sprintf("alpha - s%s$%d <= 0\n\t", p, k), sep="")
	cons2 <- paste(cons2, "isfeasible = 0 -> alpha == 0\n", sep="")

	cons3 <- "\t"
	cons3.5 <- ""
	for (i in l) {
		nms <- names(i)
		cons3cons <- ""
		cons3sum <- "\n\t"
		cons3.5cons <- ""
		cur <- paste("_", paste(nms, collapse="."), sep="")

		# make the secession variables, constraints
		# do not make a secession variable for the final merger
		if (length(i) != length(fin.merge)) {
			for (j in nms) {
				cons3tmp <- paste("secede", j, cur, " = 1 -> eps", j, final, " => ",
					i[[j]], " - ", fin.merge[[j]], " + eps", j, cur, sep="")
				cons3cons <- paste(cons3cons, cons3tmp, sep="\n\t")
				cons3sum <- paste(cons3sum, "secede", j, cur, " + ", sep="")
			}
			cons3sum <- paste(substr(cons3sum, 1, nchar(cons3sum) - 3), " => 1", sep="")
			cons3 <- paste(cons3, paste(cons3sum, cons3cons, sep="\t"), sep="\n")
		}

		# make epsilons
		for (j in nms) {
			cons3.5tmp <- paste("\n\t-eps", j, cur, " + [", sep="")
			sqs <- as.numeric(sqrt(sum(pops[i])))
			for (k in 1:4)
				for (m in nms)
					cons3.5tmp <- paste(cons3.5tmp, sprintf("%f s%s$%d * s%s$%d + ",
						pops[[m]] / sqs, j, k, m, k), sep="")

			cons3.5tmp <- paste(substr(cons3.5tmp, 1, nchar(cons3.5tmp) - 3), "] == 0\n", sep="")
			cons3.5cons <- paste(cons3.5cons, cons3.5tmp, sep="\t")
		}
		cons3.5 <- paste(cons3.5, cons3.5cons, sep="")
	}

	cons4 <- "Bounds\n"
	for (i in precs)
		for (k in 1:4)
			cons4 <- paste(cons4, sprintf("\t%f <= s%s$%d\n", lobound, i, k), sep="")
	cons4 <- paste(cons4, sprintf("\t%f <= alpha <= 0\n", lobound), sep="")

	cons5 <- "Binary\n\t"
	for (i in l)
		if (length(i) != length(fin.merge))
			for (j in names(i))
				cons5 <- paste(cons5, "secede", j, "_", paste(names(i), collapse="."), "\n\t", sep="")
	cons5 <- paste(cons5, "isfeasible\n", sep="")

	bds <- paste(cons4, cons5, sep="\n")
	cons <- paste(cons1, paste(cons3, cons3.5, cons2, sep="\n"), sep="")
	out <- paste(mns, cons, bds, "End", sep="\n")
	return(out)
}

write.cplex.file.interference <- function(l, fin.merge, pops, filename, d=getwd()) {
	stopifnot(length(fin.merge) == length(pops))
	o <- make_cplex_file_interference(l, fin.merge, pops)
	cat(o, file=paste(d, "/", filename, sep=""))
	return(o)
}

write.cplex.file.interference.alpha <- function(l, fin.merge, pops, lobound, filename, d=getwd()) {
	stopifnot(length(fin.merge) == length(pops))
	o <- make_cplex_file_interference(l, fin.merge, pops, lobound)
	cat(o, file=paste(d, "/", filename, sep=""))
	return(o)	
}
