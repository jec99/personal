	# step 1: make prefecture generation more uniform
		# increase scale?
	# remove the coastline process, just have dead indices
		# accurately represents the actual area being evalutated beforehand

	# japan area  == 338000 km^2
		# .2 deg^2 == spherical.distance(0, 0, 0, 0.2) ^ 2 ==
		# 22.26377 ^ 2 = 495.6754

		# 333800 / 495.6754 ~= 681
		# 75000 / 681 == 109.987
		# thus we must have aproximately 110 municipalities in a box
			# box being 0.2 / lat.lon.scaling x 0.2 degrees

	# 3300 / 681 ~= 5
		# so we should have about 5 municipalities in the 1.5 x 1.5 box
		# after the mergers take place.

	# note: points.min should be slightly lower in order to get more prefectures
	# with a lower height, as perimeters increases with radius but prefecture
	# count increases with the square of the radius. points.min and points.max
	# must depend on height andwidth

	# for the use of 0.3 deg^2 squares
		# 0.3 deg^2 == spherical.distance(0, 0, 0, 0.3) ^ 2 == 
		# 33.339563 ^ 2 == 1115.27
		# 338000 / 1115.27 == 303
		# 75000 / 303 == 247.5
		# 3300 / 303 == 10.89

	# for the use of 1 deg^2 squares
		# 1 deg^2 == 12391.89
		# 338000 / 12391.89 = 27.276
		# 75000 / 27.276 == 2750
		# 3300 / 27.276 == 120.98

	# for the use of 0.6 deg^2 squares
		# 4441.079
		# 338000 / 4441.079 == 76.1
		# 75000 / 76.1 == 985.5
		# 3300 / 76.1 == 43.36

	# 0.4 deg^2
		# 44.537^2 == 1982.7
		# 338000 / 1982.7 == 170.47
		# 75000 / 170.47 == 440
		# 3300 / 440 == 7.5

	# 0.2: hatred.ratio == 19
	# 0.3: hatred.ratio == 22 - 25
	# 0.4: hatred.ratio == 
library("Rcpp")
library("inline")
library("igraph")
library("bitops")
library("deldir")
library("voronoi")
library("raster")
library("sp")
library("spdep")
library("maptools")
library("rgeos")
library("orloca")
library("gpclib")
library("ucminf")
library("parallel")

proj <- "+proj=lonlat +ellps=bessel +towgs84=-147.54,507.26,680.47"

spolys <- SpatialPolygons(shp@polygons, 1:3255)
jp <- unionSpatialPolygons(spolys, rep(1, 3255), avoidUnaryUnion=TRUE)
jp <- jp@polygons[[1]]@Polygons[c(380, 381, 454, 460)]
jp.areas <- c(1.778584, 3.532327, 8.634834, 22.97935)

prec.22 <- spolys
prec.22 <- spolys[munis.from.id((20:27))]
prec.22 <- unionSpatialPolygons(prec.22, rep(1, length(munis.from.id((20:27))), avoidUnaryUnion=TRUE))
prec.22 <- prec.22@polygons[[1]]@Polygons[[19]]

hatred.ratio <- 20

points.min <- 0.01
points.max <- 0.018
points.octaves <- 2
points.persistence <- 0.7
points.scale <- 2
new.points.count <- 30

height <- 0.3
width <- 0.3

#cls <- 0
#mean.lat <- 0#
#lat.lon.scaling <- 0

fixed.municipality.cost <- 10000000
income.per.capita <- 40000

# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #

polygons.graph.testing3 <- function() {
	center <- get.center.2()
	mean.lat <<- center[2]
	lat.lon.scaling <<- cos(to.radians(mean.lat))

	points.seed <- runif(1, 0, 100)
	repeat {
		points <- simplex_poisson(height, width, new.points.count,
			points.octaves, points.scale, points.persistence, points.min,
			points.max, points.seed)
		if (length(points$x) >= 280 & length(points$x) <= 320)
			break
	}

	x <- points$x - width / 2
	y <- points$y - height / 2
	x <- x / lat.lon.scaling
	x <- x + center[1]
	y <- y + center[2]
	d <- deldir(x, y)
	dead.indices <- get.dead.indices(d, x, y)
	g <- get.delaunay.graph(d, x, y, dead.indices)

	g[dead.indices, ] <- FALSE

	spatial.pgons <- get.spatial.polygons(d, length(x), dead.indices, center)
	# using weights becuase of the minuscule size of the shapes: 6.5k of them
	# in one square as opposed to ~300
	pop.out <- extract(mesh.r, spatial.pgons, small=TRUE, weights=TRUE,
		cellnumbers=TRUE)
	cls <<- lapply(pop.out, get.pop.df)
	# pop.out <- extract(mesh.r, spatial.pgons, small=TRUE, cellnumbers=TRUE)
	# cls <<- lapply(pop.out, get.pop.df2)

	for (v in V(g)) {
		if (!V(g)[v]$dead) {
			V(g)[v]$population <- sum(cls[[v]][, "adj.pop"])
			V(g)[v]$utility <- utility.function(g, v)
		}
		V(g)[v]$membership <- v
	}

	g.merged <- merge.2(g)

	return(list(graph=g, graph.merged=g.merged))
}

merger.utility.2 <- function(g, v, n, cls, hatred.ratio, lat.lon.scaling) {
	population <- sum(V(g)[c(v, n)]$population)
	members <- unlist(V(g)[c(v, n)]$membership)

	dfout <- cls[[members[1]]]
	for (i in members[-1])
		dfout <- rbind(dfout, cls[[i]])

	if (population == 0)
		return(0)

	med <- get.med(dfout, lat.lon.scaling)
	distance.merged <- get.value(dfout, med[1], med[2])
	utility <- population * income.per.capita - fixed.municipality.cost -
		distance.merged * hatred.ratio
	return(utility)
}

get.pop.df <- function(df) {
	if (is.null(df))
		return(NULL)
	adj.pop <- df[, "value"] * df[, "weight"]
	lon <- xFromCell(mesh.r, df[, "cell"])
	lat <- yFromCell(mesh.r, df[, "cell"])
	ret <- cbind(pop=df[, "value"], weight=df[, "weight"],
		adj.pop=adj.pop, lon=lon, lat=lat)
	return(ret)
}
# debugged, works
get.med <- function(df, lat.lon.scaling) {
	if (all(df[, "adj.pop"] == 0))
		return(c(mean(df[, "lon"]), mean(df[, "lat"])))
	mean.y<- weighted.mean(df[, "lat"], df[, "adj.pop"])
	loca <- loca.p(x=df[, "lon"] * lat.lon.scaling, y=df[, "lat"], w=df[, "adj.pop"])
	mean.x <- weighted.mean(loca@x, loca@w)
	med.xy <- zsummin(loca, x=mean.x, y=mean.y, algorithm="ucminf")
	return(med.xy / c(lat.lon.scaling, 1)) 
}
# debugged, works
get.value <- function(df, x, y) {
	if (all(df[, "adj.pop"] <= 0))
		return(0)
	x <- rep(x, nrow(df))
	y <- rep(y, nrow(df))
	sum <- spherical.distance(df[, "lon"], df[, "lat"], x, y) * df[, "adj.pop"]
	return(sum(sum))
}

merge.2 <- function(g, lat.lon.scaling, cls, hatred.ratio) {
	repeat {
		changed <- FALSE
		mapping <- as.numeric(V(g))
		improvable <- which(degree(g) != 0)

		while (length(improvable) != 0) {
			improvement <- 0
			if (length(improvable) == 1) {
				v <- improvable
			} else {
				v <- sample(improvable, 1)
			}
			for (n in random.neighborhood(g, v)) {
				if (merger.utility.2(g, v, n, cls, hatred.ratio, lat.lon.scaling) >= sum(V(g)[c(v, n)]$utility)) {
					improvement <- n
					break
				}
			}
			improvable <- improvable[improvable != v]
			if (improvement) {
				improvable <- improvable[improvable != improvement]
				mapping[improvement] <- v
				changed <- TRUE
			}
		}

		g <- contract.vertices(g, mapping, vertex.attr.comb=list(population="sum",
			membership="concat", utility="first", x="first", y="first", "ignore"))

		for (v in V(g)[degree(g) > 0])
			V(g)[v]$utility <- utility.function(g, v, cls, hatred.ratio, lat.lon.scaling)

		if (!changed)
			break
	}
	return(g)
}

munis.from.id <- function(id) {
	cs <- 0
	ret <- vector(length=0)
	for (i in 1:47)
		cs <- c(cs, vcount(p.graph.l[[i]]))
	cs <- cumsum(cs)
	for (i in id)
		ret <- c(ret, (cs[i] + 1):(cs[i + 1]))
	return(ret)
}

close.to.coast.2 <- function(ctr) {
	dists <- vector(length=nrow(prec.22@coords))
	coords <- prec.22@coords
	for (i in 1:length(dists))
		dists[i] <- spherical.distance(ctr[1], ctr[2], coords[i, 1], coords[i, 2])

	if (min(dists) < spherical.distance(0, 0, 0, width) / sqrt(2)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

get.center.2 <- function() {
	repeat {
		center <- as.numeric(sample.Polygon(prec.22, 1)@coords)
		if (!close.to.coast.2(center))
			break
	}

	return(center)
}

utility.function <- function(graph, v, cls, hatred.ratio, lat.lon.scaling) {
	pop <- V(graph)[v]$population
	if (pop == 0)
		return(0)
	income <- income.per.capita * pop
	surplus <- income - fixed.municipality.cost

	members <- unlist(V(graph)[v]$membership)
	dfout <- cls[[members[1]]]
	for (i in members[-1])
		dfout <- rbind(dfout, cls[[i]])

	med <- get.med(dfout, lat.lon.scaling)
	V(graph)[v]$x <- med[1]
	V(graph)[v]$y <- med[2]
	med.value <- get.value(dfout, med[1], med[2])
	distance <- med.value * hatred.ratio

	util.t <- vector(length=4)

	t1 <- V(graph)[v]$type.1
	t2 <- V(graph)[v]$type.2
	t3 <- V(graph)[v]$type.3
	t4 <- V(graph)[v]$type.4

	util.t[1] <- (3 * t1 - t2 - t3 - t4) * t1
	util.t[2] <- (3 * t2 - t1 - t3 - t4) * t2
	util.t[3] <- (3 * t3 - t1 - t2 - t4) * t3
	util.t[4] <- (3 * t4 - t1 - t2 - t3) * t4

	type.utility <- sum(util.t) * population.factor

	return(surplus - distance + type.utility)
}

get.spatial.polygons <- function(d, n, dead.ocean, ctr, lat.lon.scaling) {
	dir <- d$dirsgs
	trv.ctr <- ctr + 2 * c(width / lat.lon.scaling, height)
	trv.mat <- rbind(trv.ctr, trv.ctr + c(0, 0.00001), trv.ctr +
		c(0.00001 / lat.lon.scaling, 0), trv.ctr)
	trivial.polygon <- Polygon(trv.mat, hole=FALSE)
	pgons <- list()
	for (i in 1:n) {
		if (i %in% dead.ocean) {
			pgons[[i]] <- Polygons(list(trivial.polygon), ID=i)
		} else {
			ppts <- get.polygon(dir, i)
			p <- Polygon(ppts, hole=FALSE)
			pgons[[i]] <- Polygons(list(p), ID=i)
		}
	}

	spatial.pgons <- SpatialPolygons(pgons, proj4string=CRS(proj))
	return(spatial.pgons)
}

# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #

test.different.hatred.ratios <- function() {
	outputs <- list()
	for (i in 1:15) {
		out <- list()
		hatred.ratio <- 10 * i + 10
		for (j in 1:10) {
			outlow <- list()
			g <- polygons.graph.testing3()
			outlow[["graphs"]] <- g
			outlow[["vertices"]] <- length(V(g$graph)[!dead])
			outlow[["unmerged"]] <- length(which(degree(g$graph.merged != 0)))
			out[[j]] <- outlow
		}
		outputs[[i]] <- out
	}
	return(outputs)
}

test.different.hatred.ratios.2.helper <- function(q) {
	if (type == "22") {
		center <- get.center.2()
	} else {
		center <- get.center()
	}

	mean.lat <- center[2]
	lat.lon.scaling <- cos(to.radians(mean.lat))

	repeat {
		points.seed <- runif(1, 0, 100)
		points <- simplex_poisson(height, width, new.points.count,
			points.octaves, points.scale, points.persistence, points.min,
			points.max, points.seed)
		if (length(points$x) >= min.vcount & length(points$x) <= max.vcount)
			break
	}

	x <- points$x - width / 2
	y <- points$y - height / 2
	x <- x / lat.lon.scaling
	x <- x + center[1]
	y <- y + center[2]
	d <- deldir(x, y)
	dead.indices <- get.dead.indices(d, x, y)
	g <- get.delaunay.graph(d, x, y, dead.indices)

	g[dead.indices, ] <- FALSE

	spatial.pgons <- get.spatial.polygons(d, length(x), dead.indices, center, lat.lon.scaling)
	# using weights becuase of the minuscule size of the shapes: 6.5k of them
	# in one square as opposed to ~300
	pop.out <- extract(mesh.r, spatial.pgons, small=TRUE, weights=TRUE,
		cellnumbers=TRUE)
	cls <- lapply(pop.out, get.pop.df)
	# pop.out <- extract(mesh.r, spatial.pgons, small=TRUE, cellnumbers=TRUE)
	# cls <<- lapply(pop.out, get.pop.df2)

	for (v in V(g)) {
		V(g)[v]$population <- sum(cls[[v]][, "adj.pop"])
		V(g)[v]$membership <- v
	}

	output <- list()
	for (i in 1:40) {
		out <- list()
		hatred.ratio <- 5 * i

		for (v in V(g))
			if (!V(g)[v]$dead)
				V(g)[v]$utility <- utility.function(g, v, cls, hatred.ratio, lat.lon.scaling)

		gp <- merge.2(g, lat.lon.scaling, cls, hatred.ratio)
		out[["merged"]] <- gp
		out[["num.unmerged"]] <- length(which(degree(gp) != 0))
		output[[i]] <- out
	}

	output[["premerge"]] <- g
	return(output)
}

test.different.hatred.ratios.2 <- function() {
	output <- list()

	type <<- "22"
	height <<- 0.3
	width <<- 0.3
	min.vcount <<- 280
	max.vcount <<- 320
	op <- list()
	for (i in 1:20)
		op[[i]] <- i
	op <- mclapply(op, test.different.hatred.ratios.2.helper,
		mc.cores=getOption("mc.cores", 4L), mc.set.seed=TRUE)
	output[["local-small"]] <- op

	type <<- "22"
	height <<- 0.6
	width <<- 0.6
	min.vcount <<- 910
	max.vcount <<- 1060
	points.min <<- 0.0105
	op <- list()
	for (i in 1:20)
		op[[i]] <- i
	op <- mclapply(op, test.different.hatred.ratios.2.helper,
		mc.cores=getOption("mc.cores", 4L), mc.set.seed=TRUE)
	output[["local-large"]] <- op

	type <<- "japan"
	height <<- 0.3
	width <<- 0.3
	min.vcount <<- 280
	max.vcount <<- 320
	points.min <<- 0.01
	op <- list()
	for (i in 1:20)
		op[[i]] <- i
	op <- mclapply(op, test.different.hatred.ratios.2.helper,
		mc.cores=getOption("mc.cores", 4L), mc.set.seed=TRUE)
	output[["national-small"]] <- op

	type <<- "japan"
	height <<- 0.6
	width <<- 0.6
	min.vcount <<- 910
	max.vcount <<- 1060
	points.min <<- 0.0105
	op <- list()
	for (i in 1:20)
		op[[i]] <- i
	op <- mclapply(op, test.different.hatred.ratios.2.helper,
		mc.cores=getOption("mc.cores", 4L), mc.set.seed=TRUE)
	output[["national-large"]] <- op

	return(output)
}

test.different.hatred.ratios.2.helper <- function(q) {
	if (type == "22") {
		center <- get.center.2()
	} else {
		center <- get.center()
	}

	mean.lat <- center[2]
	lat.lon.scaling <- cos(to.radians(mean.lat))

	repeat {
		points.seed <- runif(1, 0, 100)
		points <- simplex_poisson(height, width, new.points.count,
			points.octaves, points.scale, points.persistence, points.min,
			points.max, points.seed)
		if (length(points$x) >= min.vcount & length(points$x) <= max.vcount)
			break
	}

	x <- points$x - width / 2
	y <- points$y - height / 2
	x <- x / lat.lon.scaling
	x <- x + center[1]
	y <- y + center[2]
	d <- deldir(x, y)
	dead.indices <- get.dead.indices(d, x, y)
	g <- get.delaunay.graph(d, x, y, dead.indices)

	g[dead.indices, ] <- FALSE

	spatial.pgons <- get.spatial.polygons(d, length(x), dead.indices, center, lat.lon.scaling)
	# using weights becuase of the minuscule size of the shapes: 6.5k of them
	# in one square as opposed to ~300
	pop.out <- extract(mesh.r, spatial.pgons, small=TRUE, weights=TRUE,
		cellnumbers=TRUE)
	cls <- lapply(pop.out, get.pop.df)
	# pop.out <- extract(mesh.r, spatial.pgons, small=TRUE, cellnumbers=TRUE)
	# cls <<- lapply(pop.out, get.pop.df2)

	for (v in V(g)) {
		V(g)[v]$population <- sum(cls[[v]][, "adj.pop"])
		V(g)[v]$membership <- v
	}

	output <- list()
	for (i in 1:80) {
		out <- list()
		hatred.ratio <- 5 * i

		for (v in V(g))
			if (!V(g)[v]$dead)
				V(g)[v]$utility <- utility.function(g, v, cls, hatred.ratio, lat.lon.scaling)

		gp <- merge.2(g, lat.lon.scaling, cls, hatred.ratio)
		out[["merged"]] <- gp
		out[["num.unmerged"]] <- length(which(degree(gp) != 0))
		output[[i]] <- out
	}

	output[["premerge"]] <- g
	return(output)
}

test.different.hatred.ratios.2.smaller <- function(n.samples) {
	height <<- 0.3
	width <<- 0.3
	min.vcount <<- 280
	max.vcount <<- 320

	output <- list()

	type <<- "22"
	op <- list()
	for (i in 1:n.samples)
		op[[i]] <- i
	op <- mclapply(op, test.different.hatred.ratios.2.helper,
		mc.cores=getOption("mc.cores", 8L), mc.set.seed=TRUE)
	output[["local-small"]] <- op

	type <<- "japan"
	op <- list()
	for (i in 1:n.samples)
		op[[i]] <- i
	op <- mclapply(op, test.different.hatred.ratios.2.helper,
		mc.cores=getOption("mc.cores", 8L), mc.set.seed=TRUE)
	output[["national-small"]] <- op

	return(output)
}

# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #

get.hatred.ratios.matrix <- function(l, key) {
	len <- length(l[["local-small"]])
	dfout1 <- matrix(nrow=len, ncol=41)
	dfout2 <- matrix(nrow=len, ncol=41)

	for (i in 1:len) {
		for (j in 1:40) {
			dfout1[i, j] <- l[[key]][[i]][[j]]$num.unmerged
			dfout2[i, j] <- l[[key]][[i]][[j]]$num.unmerged
		}
		dfout1[i, 41] <- length(V(l[[key]][[i]]$premerge)[!dead])
		dfout2[i, 41] <- length(V(l[[key]][[i]]$premerge)[!dead])
	}

	dfout1 <- data.frame(dfout1)
	colnames(dfout1) <- c(as.character(seq(from=5, to=200, by=5)), "vertices")
	dfout2 <- data.frame(dfout2)
	colnames(dfout2) <- c(as.character(seq(from=5, to=200, by=5)), "vertices")

	return(list(local=dfout1, national=dfout2))
}

get.population.densities.matrix <- function(l, key, key2) {
	len <- length(l[["local"]])
	dfout <- matrix(nrow=len, ncol=80)

	for (i in 1:len) {
		for (j in 1:80) {
			dfout[i, j] <- sum(V(l[[key]][[i]][[j]][[key2]])$population)
			dfout[i, j] <- dfout[i, j] / l[[key]][[i]][[j]]$num.unmerged
		}
	}

	dfout <- data.frame(dfout)
	colnames(dfout) <- as.character(seq(from=5, to=400, by=5))

	return(dfout)
}

# -- # -- # -- # -- # -- # -- # -- # -- # -- # -- # -- #

merger.utility.1 <- function(g, v, n, cls, hatred.ratio, lat.lon.scaling) {
	population.v <- V(g)[v]$population
	population.n <- V(g)[n]$population
	util.v <- V(g)[v]$utility
	util.n <- V(g)[n]$utility
	members.v <- unlist(V(g)[v]$membership)
	members.n <- unlist(V(g)[n]$membership)

	dfout1 <- cls[[members.v[1]]]
	dfout2 <- cls[[members.n[1]]]
	for (i in members.v[-1])
		dfout1 <- rbind(dfout1, cls[[i]])
	for (j in members.n[-1])
		dfout2 <- rbind(dfout2, cls[[j]])

	if (population.v + population.n == 0)
		return(0)

	med <- get.med(rbind(dfout1, dfout2), lat.lon.scaling)

	pop.ratio.v <- population.v / (population.v + population.n)

	paid.by.v <- fixed.municipality.cost * pop.ratio.v
	surplus.v <- income.per.capita * population.v - paid.by.v
	distance.merged.v <- get.value(dfout1, med[1], med[2])
	utility.v <- surplus.v - distance.merged.v * hatred.ratio

	paid.by.n <- fixed.municipality.cost - paid.by.v
	surplus.n <- income.per.capita * population.n - paid.by.n
	distance.merged.n <- get.value(dfout2, med[1], med[2])
	utility.n <- surplus.n - distance.merged.n * hatred.ratio

	if (utility.v >= V(g)[v]$utility && utility.n >= V(g)[n]$utility) {
		return(utility.v + utility.n)
	} else {
		return(-Inf)
	}
}

merge.1 <- function(g, lat.lon.scaling, cls, hatred.ratio) {
	repeat {
		changed <- FALSE
		mapping <- as.numeric(V(g))
		improvable <- which(degree(g) != 0)

		while (length(improvable) != 0) {
			improvement <- 0
			if (length(improvable) == 1) {
				v <- improvable
			} else {
				v <- sample(improvable, 1)
			}
			for (n in random.neighborhood(g, v)) {
				if (merger.utility.1(g, v, n, cls, hatred.ratio, lat.lon.scaling) > -Inf) {
					improvement <- n
					break
				}
			}
			improvable <- improvable[improvable != v]
			if (improvement) {
				improvable <- improvable[improvable != improvement]
				mapping[improvement] <- v
				changed <- TRUE
			}
		}

		g <- contract.vertices(g, mapping, vertex.attr.comb=list(population="sum",
			membership="concat", utility="first", x="first", y="first", "ignore"))
		
		for (v in V(g)[degree(g) > 0])
			V(g)[v]$utility <- utility.function(g, v, cls, hatred.ratio, lat.lon.scaling)

		if (!changed)
			break
	}
	return(g)
}

# inputs graph, a hatred ratio, and the number of mergers to take the median of
run.back.analysis.helper <- function(l) {
	g <- l[[1]]
	hatred.ratio <- l[[2]]
	n.mergers <- l[[3]]
	x <- V(g)$x
	y <- V(g)$y
	d <- deldir(x, y)
	dead.indices <- which(degree(g) == 0)
	center <- c(V(g)$x[1], V(g)$y[1])
	spgons <- get.spatial.polygons(d, length(x),
		dead.indices, center, cos(to.radians(median(y))))

	pop.out <- extract(mesh.r, spgons, small=TRUE, weights=TRUE,
		cellnumbers=TRUE)
	cls <<- lapply(pop.out, get.pop.df)

	output <- list()
	tmp <- list()

	for (i in 1:n.mergers) {
		tmp[["merged"]] <- merge.1(g, cos(to.radians(median(y))), cls, hatred.ratio)
		tmp[["num.unmerged"]] <- length(which(degree(tmp[["merged"]]) != 0))
		output[[i]] <- tmp
	}

	# returns a list, the elements of which are pairs of merged graphs and the
	# number of unmerged vertices in those graphs
	return(output)
}

# inputs a list of graphs and hatred ratios and outputs their merged counterparts
run.back.analysis <- function(oput, l, hr, n.mergers) {
	out <- list()
	len <- length(l)
	outd <- matrix(nrow=len, ncol=(n.mergers + 2))

	for (i in 1:len) {
		out[[i]] <- list(oput[["local-small"]][[l[i]]][["premerge"]], hr[i], n.mergers)
		outd[i, n.mergers + 2] <- vcount(oput[["local-small"]][[l[i]]][["premerge"]])
	}

	out <- mclapply(out, run.back.analysis.helper,
		mc.cores=getOption("mc.cores", 4L), mc.set.seed=TRUE)

	for (i in 1:len) {
		for (j in 1:n.mergers) {
			outd[i, j] <- out[[i]][[j]][["num.unmerged"]]
		}
		outd[i, n.mergers + 1] <- median(outd[i, 1:n.mergers])		
	}

	colnames(outd) <- c(as.character(1:n.mergers), "median", "vertices")

	return(list(graphs=out, data=outd))
}

compare.municipality.counts <- function(out, analyzed) {
	len <- length(out)
	mout <- matrix(nrow=len, ncol=5)
	for (i in 1:len) {
		mout[i, 1] <- length(which(degree(out[[i]]$premerge) != 0))
		mout[i, 2] <- out[[i]]$num.unmerged
		mout[i, 3] <- analyzed[["data"]][i, "median"]
		mout[i, 4] <- mout[i, 2] / mout[i, 3]
		mout[i, 5] <- mout[i, 2] - mout[i, 3]
	}
	colnames(mout) <- c("premerge.vertices", "dictatorship.vertices",
		"democracy.vertices", "quotient", "difference")
	return(mout)
}

extract.premerged.graphs <- function(out1, out2) {
	l1 <- length(out1$[["national-small"]])
	l2 <- length(out2$[["national-small"]])
	ls <- list()
	j <- 1
	for (i in 1:l1) {
		ls[[j]] <- out1[["national-small"]][[i]][["premerge"]]
		j <- j + 1
	}
	for (i in 1:l2) {
		ls[[j]] <- out2[["national-small"]][[i]][["premerge"]]
		j <- j + 1
	}
	return(ls)
}

