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

proj <- "+proj=lonlat +ellps=bessel +towgs84=-147.54,507.26,680.47"

# requires p.graph.l, shp, and mesh.r to be loaded

points.octaves <- 2
points.scale <- 0.1
points.min <- 0.018
points.max <- 0.13
points.persistence <- 0.2
new.points.count <- 20

circle.octaves <- 2
circle.persistence <- 1
circle.scale <- 8
circle.min <- -0.75
circle.max <- 0.75
circle.rad <- 2
radius.factor <- 14

height <- 0.3
width <- 0.3

hatred.ratio <- 100
population.factor <- 1

fixed.municipality.cost <- 10000000
income.per.capita <- 40000

spolys <- SpatialPolygons(shp@polygons, 1:3255)
jp <- unionSpatialPolygons(spolys, rep(1, 3255), avoidUnaryUnion=TRUE)
jp <- jp@polygons[[1]]@Polygons[c(380, 381, 454, 460)]
jp.areas <- c(1.778584, 3.532327, 8.634834, 22.97935)

prec.22 <- spolys
prec.22 <- spolys[munis.from.id((20:27))]
prec.22 <- unionSpatialPolygons(prec.22, rep(1, length(munis.from.id((20:27))), avoidUnaryUnion=TRUE))
prec.22 <- prec.22@polygons[[1]]@Polygons[[19]]

type <- "national"  # local

# operation:
# - points.min and points.max determine the number of principal municipalities
# 	a tighter spread means a more uniform initial distribution, while a looser
# 	spread means more irregular. moving them together will increase or decrease
# 	the number of principal municipalities. this number is extremely sensitive
# 	to both, but especially the lower bound. adjust in increments of 0.005.
# - radius.factor controls how the border of the prefecture changes with number
# 	of requested municipalities, i.e. pushing the boundary out or in. it behaves
# 	in the natural way.
# - hatred.ratio is the ratio of hatred of taxes to distance.
# - population.factor controls how much people value being with people of their
# 	same type, measured in the natural units: a person gains three utility from
# 	being with another person of their type and loses one for being with a
# 	person of another type.
# - height and width should have the same value. should the program run slowly,
#		lower height and width (make sure to change radius.factor, points.min,
#		and points.max accordingly so as to have a proper boundary and the correct
#		number of initial municipalities). the upper bound for time-feasible
# 	height and width appears to be aroun 0.85; while there are feasible centers
# 	for higher values the polygon sampling function appears not to select them.
# - type indicates where the center will be places. "national" means that the
# 	center selection algorithm will use as a domain the four main islands of
# 	japan, while "local" (or anything else for that matter) means that the
# 	algorithm will use the prefectures immediately surrounding prefecture 22.
# 	the latter is meant to give a more stable, albeit more concentrated,
# 	population distribution. consequently there will be fewer mergers.
# - other generation constants have been calibrated and should not be changed.

inc <- '
	#include <vector>
	#include <list>
	#include <algorithm>
	#include <cctype>
	#include <cstdlib>
	#include <iterator>
	#include <numeric>
	#include <cstddef>
	#include <memory>
	#include <math.h>

	#define PI (3.1415926535)

	Rcpp::RNGScope scope;

	int fastfloor(const double x);
	double dot(const int *g, const double x, const double y, const double z);

	double random_real(double n, double k) {
	  NumericVector x = runif(1, n, k);
	  return x[0];
	}

	int random_int(int n) {
	  return fastfloor(random_real(0.0, (double) n));
	}

	double octave_noise(const double octaves,
	                    const double persistence,
	                    const double scale,
	                    const double x,
	                    const double y,
	                    const double z);

	double scaled_octave_noise(const double octaves,
	                           const double persistence,
	                           const double scale,
	                           const double low_bound,
	                           const double high_bound,
	                           const double x,
	                           const double y,
	                           const double z);

	double scaled_raw_noise(const double low_bound,
	                        const double high_bound,
	                        const double x,
	                        const double y,
	                        const double z);

	double raw_noise(const double x,
	                 const double y,
	                 const double z);

	const int grad3[12][3] = {
	  { 1, 1, 0 }, { -1, 1, 0 }, { 1, -1, 0 }, { -1, -1, 0 },
	  { 1, 0, 1 }, { -1, 0, 1 }, { 1, 0, -1 }, { -1 ,0 , -1 },
	  { 0, 1, 1 }, { 0 , -1, 1 }, { 0, 1, -1 }, { 0 ,-1 , -1 }
	};

	const int perm[512] = {
	  151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142,
	  8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117,
	  35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71,
	  134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41,
	  55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208,  89,
	  18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226,
	  250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182,
	  189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43,
	  172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97,
	  228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239,
	  107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
	  138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180,

	  151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142,
	  8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117,
	  35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71,
	  134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41,
	  55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208,  89,
	  18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226,
	  250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182,
	  189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43,
	  172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97,
	  228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239,
	  107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
	  138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180
	};

	double octave_noise(const double octaves, const double persistence, const double scale, const double x, const double y, const double z) {
	  double total = 0;
	  double frequency = scale;
	  double amplitude = 1;
	  double max_amplitude = 0;

	  for (int i = 0; i < octaves; ++i) {
	    total += raw_noise(x * frequency, y * frequency, z * frequency) * amplitude;
	    frequency *= 2;
	    max_amplitude += amplitude;
	    amplitude *= persistence;
	  }

	  return total / max_amplitude;
	}

	double scaled_octave_noise(const double octaves, const double persistence, const double scale, const double low_bound, const double high_bound, const double x, const double y, const double z) {
	  return octave_noise(octaves, persistence, scale, x, y, z) * (high_bound - low_bound) / 2.0 + (high_bound + low_bound) / 2.0;
	}

	double scaled_raw_noise(const double low_bound, const double high_bound, const double x, const double y, const double z) {
	  return raw_noise(x, y, z) * (high_bound - low_bound) / 2 +
	    (high_bound + low_bound) / 2;
	}

	double raw_noise(const double x, const double y, const double z) {
	  double n0, n1, n2, n3;

	  double F3 = 1.0 / 3.0;
	  double s = (x + y + z) * F3;
	  int i = fastfloor(x + s);
	  int j = fastfloor(y + s);
	  int k = fastfloor(z + s);

	  double G3 = 1.0 / 6.0;
	  double t = (i + j + k) * G3;
	  double X0 = i - t;
	  double Y0 = j - t;
	  double Z0 = k - t;
	  double x0 = x - X0;
	  double y0 = y - Y0;
	  double z0 = z - Z0;

	  int i1, j1, k1;
	  int i2, j2, k2;

	  if (x0 >= y0) {
	    if (y0 >= z0) {
	      i1 = 1;
	      j1 = 0;
	      k1 = 0;
	      i2 = 1;
	      j2 = 1;
	      k2 = 0;
	    } else if (x0 >= z0) {
	      i1 = 1;
	      j1 = 0;
	      k1 = 0;
	      i2 = 1;
	      j2 = 0;
	      k2 = 1;
	    } else {
	      i1 = 0;
	      j1 = 0;
	      k1 = 1;
	      i2 = 1;
	      j2 = 0;
	      k2 = 1;
	    }
	  } else {
	    if (y0 < z0) {
	      i1 = 0;
	      j1 = 0;
	      k1 = 1;
	      i2 = 0;
	      j2 = 1;
	      k2 = 1;
	    } else if (x0 < z0) {
	      i1 = 0;
	      j1 = 1;
	      k1 = 0;
	      i2 = 0;
	      j2 = 1;
	      k2 = 1;
	    } else {
	      i1=0;
	      j1=1;
	      k1=0;
	      i2=1;
	      j2=1;
	      k2=0;
	    }
	  }

	  double x1 = x0 - i1 + G3;
	  double y1 = y0 - j1 + G3;
	  double z1 = z0 - k1 + G3;
	  double x2 = x0 - i2 + 2.0 * G3;
	  double y2 = y0 - j2 + 2.0 * G3;
	  double z2 = z0 - k2 + 2.0 * G3;
	  double x3 = x0 - 1.0 + 3.0 * G3;
	  double y3 = y0 - 1.0 + 3.0 * G3;
	  double z3 = z0 - 1.0 + 3.0 * G3;

	  int ii = i & 255;
	  int jj = j & 255;
	  int kk = k & 255;
	  int gi0 = perm[ii + perm[jj + perm[kk]]] % 12;
	  int gi1 = perm[ii + i1 + perm[jj + j1 + perm[kk + k1]]] % 12;
	  int gi2 = perm[ii + i2 + perm[jj + j2 + perm[kk + k2]]] % 12;
	  int gi3 = perm[ii + 1 + perm[jj + 1 + perm[kk + 1]]] % 12;

	  double t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0;
	  if (t0 < 0) n0 = 0.0;
	  else {
	    t0 *= t0;
	    n0 = t0 * t0 * dot(grad3[gi0], x0, y0, z0);
	  }

	  double t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1;
	  if (t1 < 0) n1 = 0.0;
	  else {
	    t1 *= t1;
	    n1 = t1 * t1 * dot(grad3[gi1], x1, y1, z1);
	  }

	  double t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2;
	  if (t2 < 0) n2 = 0.0;
	  else {
	    t2 *= t2;
	    n2 = t2 * t2 * dot(grad3[gi2], x2, y2, z2);
	  }

	  double t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3;
	  if (t3 < 0) n3 = 0.0;
	  else {
	    t3 *= t3;
	    n3 = t3 * t3 * dot(grad3[gi3], x3, y3, z3);
	  }

	  return 32.0 * (n0 + n1 + n2 + n3);
	}

	int fastfloor(const double x) {
	  return x > 0 ? (int) x : (int) (x - 1);
	}

	double dot(const int *g, const double x, const double y, const double z) {
	  return g[0] * x + g[1] * y + g[2] * z;
	}

	class Point {
	public:
	  double x, y;

	  Point(double why, double ecks): y(why), x(ecks) { }
	  Point(): y(-1.0), x(-1.0) { }

	  Point operator*(Point p) const {
	    return Point(y * p.y, x * p.x);
	  }

	  Point operator*(double d) const {
	    return Point(y * d, x * d);
	  }

	  Point operator+(Point p) const {
	    return Point(y + p.y, x + p.x);
	  }

	  Point operator-(Point p) const {
	    return Point(y - p.y, x - p.x);
	  }

	  bool is_null() {
	    return (x == -1 && y == -1);
	  }
	};

	template <typename T>
	class RandomQueue {
	  std::list<T> pts;
	public:
	  RandomQueue(): pts() { }

	  void push(T a) {
	    pts.push_back(a);
	  }

	  T pop() {
	    int whr = random_int(pts.size());
	    typename std::list<T>::iterator it = pts.begin();
	    advance(it, whr);
	    T ret = *it;
	    pts.erase(it);
	    return ret;
	  }

	  bool empty() const {
	    return pts.empty();
	  }
	};

	template <typename T>
	class Grid {
	  std::vector<std::vector<std::vector<T> > > g;
	  int height;
	  int width;
	public:
	  Grid(int h, int w): height(h), width(w) {
	    g.resize(height);
	    for (int i = 0; i < g.size(); ++i)
	      g[i].resize(width);
	    std::vector<T> ls(1, T(h, w));
	    ls.clear();
	    for (int i = 0; i < g.size(); ++i)
	      for (int j = 0; j < g[i].size(); ++j)
	        g[i][j] = ls;
	  }

	  std::vector<T> operator[](T p) const {
	    int x = (int) p.x;
	    int y = (int) p.y;
	    if (x >= width || y >= height || x < 0 || y < 0)
	      return std::vector<T>();
	    else
	      return g[y][x];
	  }

	  void add(T p, T q) {
	    int x = (int) p.x;
	    int y = (int) p.y;
	    if (x >= width || y >= height || x < 0 || y < 0) return;
	    g[y][x].push_back(q);
	  }

	  std::vector<T> cells_around(T p, int n) {
	    std::vector<T> ret;
	    int x = (int) p.x;
	    int y = (int) p.y;
	    n = int (n / 2);
	    for (int i = y - n; i < y + n + 1; ++i)
	      for (int j = x - n; j < x + n + 1; ++j)
	        if (i >= height || j >= width || i < 0 || j < 0)
	          continue;
	        else
	          ret.insert(ret.end(), g[i][j].begin(), g[i][j].end());
	    return ret;
	  }
	};

	template <typename Pt>
	Pt image_to_grid(Pt p, double cellsize) {
	  int x = (int) (p.x / cellsize);
	  int y = (int) (p.y / cellsize);
	  return Pt(y, x);
	}

	template <typename Pt>
	double distance(Pt a, Pt b) {
	  return(sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2)));
	}

	template <typename Pt>
	Pt generateRandomPointAround(Pt p, double min_dist) {
	  double r1, r2, radius, angle;
	  r1 = random_real(1.0, 2.0);
	  r2 = random_real(0.0, 1.0);
	  radius = min_dist * r1;
	  angle = 2.0 * PI * r2;
	  return Pt(p.y + sin(angle) * radius, p.x + cos(angle) * radius);
	}

	template <typename Pt>
	bool in_rectangle(double height, double width, Pt p) {
	  double x = p.x;
	  double y = p.y;
	  return (x < width && x > 0 && y < height && y > 0);
	}

	template <typename G, typename P>
	bool in_neighborhood(G g, P p, double min_dist, double cellsize, int height, int width) {
	  P gridpoint = image_to_grid(p, cellsize);
	  std::vector<P> pts = g.cells_around(gridpoint, 5);
	  for (typename std::vector<P>::iterator i = pts.begin(); i != pts.end(); ++i)
	    if (distance(*i, p) < min_dist)
	      return true;
	  return false;
	}

	std::vector<Point> poisson_sample(double height, double width, int new_points_count, double octaves, double persistence, double scale, double low, double high, double seed) {
	  double cellsize = high / sqrt(2.0);
	  Grid<Point> grid(ceil(height / cellsize), ceil(width / cellsize));
	  RandomQueue<Point> processing_list;
	  std::vector<Point> output_list;
	  Point p, q;
	  double min_dist;

	  Point first(random_real(0.0, height), random_real(0.0, width));
	  processing_list.push(first);
	  output_list.push_back(first);

	  grid.add(image_to_grid(first, cellsize), first);

	  while (!processing_list.empty()) {
	    p = processing_list.pop();
	    for (int i = 0; i < new_points_count; ++i) {
	      q = generateRandomPointAround(p, min_dist);
	      min_dist = scaled_octave_noise(octaves, persistence, scale, low, high, q.x, q.y, seed);
	      if (in_rectangle(height, width, q) && !in_neighborhood(grid, q, min_dist, cellsize, height, width)) {
	        processing_list.push(q);
	        output_list.push_back(q);
	        grid.add(image_to_grid(q, cellsize), q);
	      }
	    }
	  }

	  return output_list;
	}
'

src <- '
	double ht = Rcpp::as<double>(height);
  double wd = Rcpp::as<double>(width);
  double np = Rcpp::as<double>(newpoints);

  double oc = Rcpp::as<double>(octaves);
  double ps = Rcpp::as<double>(persistence);
  double sc = Rcpp::as<double>(scale);
  double lo = Rcpp::as<double>(low);
  double hi = Rcpp::as<double>(high);
  double sd = Rcpp::as<double>(seed);

  std::vector<Point> ret = poisson_sample(ht, wd, np, oc, ps, sc, lo, hi, sd);
  Rcpp::NumericVector xret;
  Rcpp::NumericVector yret;

  for (std::vector<Point>::iterator i = ret.begin(); i != ret.end(); ++i) {
    xret.push_back((*i).x);
    yret.push_back((*i).y);
  }

  return Rcpp::List::create(Rcpp::Named("x", xret), Rcpp::Named("y", yret));
'

simplex_poisson <- cxxfunction(signature(height="numeric",
																				 width="numeric",
																				 newpoints="numeric",
																				 octaves="numeric",
																				 persistence="numeric",
																				 scale="numeric",
																				 low="numeric",
																				 high="numeric",
																				 seed="numeric"),
															 					 body=src,
															 					 include=inc,
															 					 plugin="Rcpp",
															 					 cppargs="-I/usr/include")

inc2 <- '
	double octave_noise(const double octaves,
										const double persistence,
										const double scale,
										const double x,
										const double y,
										const double z);

	double scaled_octave_noise(const double octaves,
									 				   const double persistence,
										 			   const double scale,
										 			   const double low_bound,
										 			   const double high_bound,
											 		   const double x,
												 	   const double y,
												 	   const double z);

	double scaled_raw_noise(const double low_bound,
													const double high_bound,
													const double x,
													const double y,
													const double z);

	double raw_noise(const double x,
									 const double y,
									 const double z);

	int fastfloor(const double x);
	double dot(const int *g, const double x, const double y, const double z);

	const int grad3[12][3] = {
		{ 1, 1, 0 }, { -1, 1, 0 }, { 1, -1, 0 }, { -1, -1, 0 },
		{ 1, 0, 1 }, { -1, 0, 1 }, { 1, 0, -1 }, { -1 ,0 , -1 },
		{ 0, 1, 1 }, { 0 , -1, 1 }, { 0, 1, -1 }, { 0 ,-1 , -1 }
	};

	const int perm[512] = {
		151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142,
		8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117,
		35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71,
		134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41,
		55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208,  89,
		18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226,
		250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182,
		189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43,
		172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97,
		228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239,
		107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
		138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180,

		151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142,
		8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117,
		35, 11, 32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71,
		134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41,
		55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208,  89,
		18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226,
		250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182,
		189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43,
		172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97,
		228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239,
		107, 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
		138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180
	};

	double octave_noise(const double octaves, const double persistence, const double scale, const double x, const double y, const double z) {
	  double total = 0;
	  double frequency = scale;
	  double amplitude = 1;
	  double max_amplitude = 0;

	  for (int i = 0; i < octaves; ++i) {
	    total += raw_noise(x * frequency, y * frequency, z * frequency) * amplitude;
	    frequency *= 2;
	    max_amplitude += amplitude;
	    amplitude *= persistence;
	  }

	  return total / max_amplitude;
	}

	double scaled_octave_noise(const double octaves, const double persistence, const double scale, const double low_bound, const double high_bound, const double x, const double y, const double z) {
	  return octave_noise(octaves, persistence, scale, x, y, z) * (high_bound - low_bound) / 2.0 + (high_bound + low_bound) / 2;
	}

	double scaled_raw_noise(const double low_bound, const double high_bound, const double x, const double y, const double z) {
	  return raw_noise(x, y, z) * (high_bound - low_bound) / 2 +
	  	(high_bound + low_bound) / 2;
	}

	double raw_noise(const double x, const double y, const double z) {
	  double n0, n1, n2, n3;

	  double F3 = 1.0 / 3.0;
	  double s = (x + y + z) * F3;
	  int i = fastfloor(x + s);
	  int j = fastfloor(y + s);
	  int k = fastfloor(z + s);

	  double G3 = 1.0 / 6.0;
	  double t = (i + j + k) * G3;
	  double X0 = i - t;
	  double Y0 = j - t;
	  double Z0 = k - t;
	  double x0 = x - X0;
	  double y0 = y - Y0;
	  double z0 = z - Z0;

	  int i1, j1, k1;
	  int i2, j2, k2;

	  if (x0 >= y0) {
	    if (y0 >= z0) {
	    	i1 = 1;
	    	j1 = 0;
	    	k1 = 0;
	    	i2 = 1;
	    	j2 = 1;
	    	k2 = 0;
	    } else if (x0 >= z0) {
	    	i1 = 1;
	    	j1 = 0;
	    	k1 = 0;
	    	i2 = 1;
	    	j2 = 0;
	    	k2 = 1;
	    } else {
	    	i1 = 0;
	    	j1 = 0;
	    	k1 = 1;
	    	i2 = 1;
	    	j2 = 0;
	    	k2 = 1;
	    }
	  } else {
	    if (y0 < z0) {
	      i1 = 0;
	      j1 = 0;
	      k1 = 1;
	      i2 = 0;
	      j2 = 1;
	      k2 = 1;
	    } else if (x0 < z0) {
	    	i1 = 0;
	    	j1 = 1;
	      k1 = 0;
	      i2 = 0;
	      j2 = 1;
	      k2 = 1;
	    } else {
	    	i1=0;
	      j1=1;
	      k1=0;
	      i2=1;
	      j2=1;
	      k2=0;
	    }
	  }

	  double x1 = x0 - i1 + G3;
	  double y1 = y0 - j1 + G3;
	  double z1 = z0 - k1 + G3;
	  double x2 = x0 - i2 + 2.0 * G3;
	  double y2 = y0 - j2 + 2.0 * G3;
	  double z2 = z0 - k2 + 2.0 * G3;
	  double x3 = x0 - 1.0 + 3.0 * G3;
	  double y3 = y0 - 1.0 + 3.0 * G3;
	  double z3 = z0 - 1.0 + 3.0 * G3;

	  int ii = i & 255;
	  int jj = j & 255;
	  int kk = k & 255;
	  int gi0 = perm[ii + perm[jj + perm[kk]]] % 12;
	  int gi1 = perm[ii + i1 + perm[jj + j1 + perm[kk + k1]]] % 12;
	  int gi2 = perm[ii + i2 + perm[jj + j2 + perm[kk + k2]]] % 12;
	  int gi3 = perm[ii + 1 + perm[jj + 1 + perm[kk + 1]]] % 12;

	  double t0 = 0.6 - x0 * x0 - y0 * y0 - z0 * z0;
	  if (t0 < 0) n0 = 0.0;
	  else {
	    t0 *= t0;
	    n0 = t0 * t0 * dot(grad3[gi0], x0, y0, z0);
	  }

	  double t1 = 0.6 - x1 * x1 - y1 * y1 - z1 * z1;
	  if (t1 < 0) n1 = 0.0;
	  else {
	    t1 *= t1;
	    n1 = t1 * t1 * dot(grad3[gi1], x1, y1, z1);
	  }

	  double t2 = 0.6 - x2 * x2 - y2 * y2 - z2 * z2;
	  if (t2 < 0) n2 = 0.0;
	  else {
	    t2 *= t2;
	    n2 = t2 * t2 * dot(grad3[gi2], x2, y2, z2);
	  }

	  double t3 = 0.6 - x3 * x3 - y3 * y3 - z3 * z3;
	  if (t3 < 0) n3 = 0.0;
	  else {
	    t3 *= t3;
	    n3 = t3 * t3 * dot(grad3[gi3], x3, y3, z3);
	  }

	  return 32.0 * (n0 + n1 + n2 + n3);
	}

	int fastfloor(const double x) {
		return x > 0 ? (int) x : (int) (x - 1);
	}

	double dot(const int *g, const double x, const double y, const double z) {
		return g[0] * x + g[1] * y + g[2] * z;
	}
'

src2 <- '
	int oct = Rcpp::as<int>(o);
	double prs = Rcpp::as<double>(p);
	double scl = Rcpp::as<double>(s);
	double min = Rcpp::as<double>(mn);
	double max = Rcpp::as<double>(mx);
	double x = Rcpp::as<double>(ex);
	double y = Rcpp::as<double>(why);
	double z = Rcpp::as<double>(seed);

	return Rcpp::wrap(scaled_octave_noise(oct, prs, scl, min, max, x, y, z));
'

simplex_noise <- cxxfunction(signature(o="numeric",
																 p="numeric",
																 s="numeric",
																 mn="numeric",
																 mx="numeric",
																 ex="numeric",
																 why="numeric",
																 seed="numeric"),
											 					 body=src2,
											 					 include=inc2,
											 					 plugin="Rcpp",
											 					 cppargs="-I/usr/include")

to.radians <- function(n) {
	return(n / 180 * pi)
}

spherical.distance <- function(lon1, lat1, lon2, lat2) {
	lon1 <- to.radians(lon1)
	lat1 <- to.radians(lat1)
	lon2 <- to.radians(lon2)
	lat2 <- to.radians(lat2)
	return((2 * asin(sqrt((sin((lat1 - lat2) / 2)) ^ 2 + cos(lat1) * cos(lat2) * (sin((lon1 - lon2) / 2)) ^ 2))) * 6378.1)
}

close.to.coast <- function(ctr, rndm.isld) {
	coords <- jp[[rndm.isld]]@coords
	ctr <- matrix(rep(ctr, nrow(jp[[rndm.isld]]@coords)), byrow=TRUE, ncol=2)
	dists <- spherical.distance(ctr[, 1], ctr[, 2], coords[, 1], coords[, 2])

	if (min(dists) < spherical.distance(0, 0, 0, width) / sqrt(2)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

get.center <- function() {
	random.center <- runif(1, 0, sum(jp.areas))
	random.island <- 1
	while (random.center > (cumsum(jp.areas)[random.island]))
		random.island <- random.island + 1

	repeat {
		center <- as.numeric(sample.Polygon(jp[[random.island]], 1)@coords)
		if (!close.to.coast(center, random.island))
			break
	}

	return(center)
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
	coords <- prec.22@coords
	ctr <- matrix(rep(ctr, nrow(prec.22@coords)), byrow=TRUE, ncol=2)
	dists <- spherical.distance(ctr[, 1], ctr[, 2], coords[, 1], coords[, 2])

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

get.dead.indices <- function(d, x, y) {
	dead.indices <- vector(length=0)
	for (i in 1:length(x)) {
		dir <- d$dirsgs[d$dirsgs[, "ind1"] == i | d$dirsgs[, "ind2"] == i, ]
		if (any(dir[, "bp1"] | dir[, "bp2"]))
			dead.indices <- c(dead.indices, i)
	}
	return(dead.indices)
}

interleave <- function(x, y) {
	ord1 <- 2 * (1:length(x)) - 1
	ord2 <- 2 * (1:length(y))
	c(x,y)[order(c(ord1,ord2))]
}

get.delaunay.graph <- function(d, x, y, dead.indices) {
	g.delaunay <- graph.empty(directed=FALSE) + vertices(1:length(x), x=x, y=y, ocean=FALSE, dead=FALSE)
	g.delaunay <- g.delaunay + edges(interleave(as.numeric(d$delsgs[, "ind1"]), as.numeric(d$delsgs[, "ind2"])))

	V(g.delaunay)$membership <- 1:length(x)
	V(g.delaunay)[dead.indices]$dead <- TRUE
	V(g.delaunay)[dead]$ocean <- TRUE
	return(g.delaunay)
}

simplex.circle.interpolation <- function(s, y, z, begin, end, theta) {
	mid <- (begin - end) / 2
	theta <- s + 1 / (2 * pi) * theta
	# theta is now in [s, s + 1]
	out <- simplex_noise(circle.octaves, circle.persistence,
		circle.scale, circle.min, circle.max, theta, y, z) + (2 * (theta - s) - 1)
	# out <- out + ((theta - s) - (s + 1 - theta)) * mid
	# out <- out + (2 * (theta - s) - 1)
	return(out)
}

generate.coastline <- function(g, n, ctr, lat.lon.scaling) {
	# generate a "wobbly" circle using the simplex.circle function
	w <- runif(1, 0, 10)
	z <- runif(1, 0, 10)
	repeat {
		s <- runif(1, 0, 10)
		begin <- simplex_noise(circle.octaves, circle.persistence,
			circle.scale, circle.min, circle.max, s, w, z)
		end <- simplex_noise(circle.octaves, circle.persistence,
			circle.scale, circle.min, circle.max, s + 1, w, z)

		if (abs(begin - end) < 0.4)
			break
	}

	rad <- sqrt(n) * radius.factor
	for (v in V(g)) {
		x <- V(g)[v]$x
		y <- V(g)[v]$y
		len <- spherical.distance(x, y, ctr[1], ctr[2])
		x <- x * lat.lon.scaling
		theta <- atan2(y - ctr[2], x - ctr[1])
		theta <- theta + 2 * pi * (theta < 0)

		V(g)[v]$ocean <- (len > rad +
			simplex.circle.interpolation(s, w, z, begin, end, theta) *
			radius.factor) & (!V(g)[v]$dead)
	}

	return(g)
}

get.polygon <- function(df, i) {
	wch1 <- which(df[, "ind1"] == i) # find entries relating to point i
	wch2 <- which(df[, "ind2"] == i) # aka the i-voronoi-cell
	wch <- unique(c(wch1, wch2)) # no self-loops, but just a safety
	d <- df[wch, ]
	# 1 = x1, 2 = y1, etc. for looping and equality testing
	d <- cbind(as.numeric(d[, "x1"]), as.numeric(d[, "y1"]),
		as.numeric(d[, "x2"]), as.numeric(d[, "y2"]))
	# we can choose a random starting point because we know we are not on
	# the edge of the window
	x1 <- d[1, 3]
	y1 <- d[1, 4]
	poly <- matrix(nrow=0, ncol=2)
	poly <- rbind(poly, c(x1, y1))

	# basically, sort them
	while (!is.null(nrow(d))) {
		w <- which(d[, 1] == x1 & d[, 2] == y1)
		if (length(w) != 0) {
			x1 <- d[w[1], 3]
			y1 <- d[w[1], 4]
			poly <- rbind(poly, c(x1, y1))
			d <- d[-w[1], ]
		} else {
			w <- which(d[, 3] == x1 & d[, 4] == y1)
			x1 <- d[w[1], 1]
			y1 <- d[w[1], 2]
			poly <- rbind(poly, c(x1, y1))
			d <- d[-w[1], ]
		}
	}

	poly <- rbind(poly, poly[1, ])
	return(poly)
}

get.spatial.polygons <- function(d, n, dead.ocean, ctr, lat.lon.scaling) {
	trv.ctr <- ctr + 2 * c(width / lat.lon.scaling, height)
	trv.mat <- rbind(trv.ctr, trv.ctr + c(0, 0.00001), trv.ctr +
		c(0.00001 / lat.lon.scaling, 0), trv.ctr)
	trivial.polygon <- Polygon(trv.mat, hole=FALSE)
	pgons <- list()

	for (i in 1:n) {
		if (i %in% dead.ocean) {
			pgons[[i]] <- Polygons(list(trivial.polygon), ID=i)
		} else {
			ppts <- get.polygon(d$dirsgs, i)
			p <- Polygon(ppts, hole=FALSE)
			pgons[[i]] <- Polygons(list(p), ID=i)
		}
	}

	spatial.pgons <- SpatialPolygons(pgons, proj4string=CRS(proj))
	return(spatial.pgons)
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

utility.function <- function(g, v, cls, hatred.ratio, lat.lon.scaling) {
	pop <- V(g)[v]$population
	if (pop == 0)
		return(0)
	surplus <- income.per.capita * pop - fixed.municipality.cost

	members <- unlist(V(g)[v]$membership)
	dfout <- cls[[members[1]]]
	for (i in members[-1])
		dfout <- rbind(dfout, cls[[i]])

	med <- get.med(dfout, lat.lon.scaling)
	V(g)[v]$x <- med[1]
	V(g)[v]$y <- med[2]
	med.value <- get.value(dfout, med[1], med[2])
	distance <- med.value * hatred.ratio

	util.t <- vector(length=4)

	t1 <- V(g)[v]$type.1
	t2 <- V(g)[v]$type.2
	t3 <- V(g)[v]$type.3
	t4 <- V(g)[v]$type.4

	util.t[1] <- (3 * t1 - t2 - t3 - t4) * t1
	util.t[2] <- (3 * t2 - t1 - t3 - t4) * t2
	util.t[3] <- (3 * t3 - t1 - t2 - t4) * t3
	util.t[4] <- (3 * t4 - t1 - t2 - t3) * t4

	type.utility <- sum(util.t) * population.factor

	return(surplus - distance + type.utility)
}

get.med <- function(df, lat.lon.scaling) {
	if (all(df[, "adj.pop"] == 0))
		return(c(mean(df[, "lon"]), mean(df[, "lat"])))
	mean.y <- weighted.mean(df[, "lat"], df[, "adj.pop"])
	loca <- loca.p(x=df[, "lon"] * lat.lon.scaling, y=df[, "lat"], w=df[, "adj.pop"])
	mean.x <- weighted.mean(loca@x, loca@w)
	med.xy <- zsummin(loca, x=mean.x, y=mean.y, algorithm="ucminf")
	return(med.xy / c(lat.lon.scaling, 1))
}

get.value <- function(df, x, y) {
	if (all(df[, "adj.pop"] <= 0))
		return(0)
	x <- rep(x, nrow(df))
	y <- rep(y, nrow(df))
	sum <- spherical.distance(df[, "lon"], df[, "lat"], x, y) * df[, "adj.pop"]
	return(sum(sum))
}

random.neighborhood <- function(g, v, improvable) {
	nbd <- unlist(neighborhood(g, v, order=1))
	nbd <- nbd[nbd != v]
	nbd <- nbd[which(nbd %in% improvable)]
	if (length(nbd) < 2) {
		return(nbd)
	} else {
		return(sample(nbd))
	}
}

merger.utility.1 <- function(g, v, n, cls, hatred.ratio, lat.lon.scaling) {
	population.v <- V(g)[v]$population
	population.n <- V(g)[n]$population
	if (population.v + population.n == 0)
		return(0)

	members.v <- unlist(V(g)[v]$membership)
	members.n <- unlist(V(g)[n]$membership)

	dfout1 <- cls[[members.v[1]]]
	dfout2 <- cls[[members.n[1]]]
	for (i in members.v[-1])
		dfout1 <- rbind(dfout1, cls[[i]])
	for (j in members.n[-1])
		dfout2 <- rbind(dfout2, cls[[j]])

	med <- get.med(rbind(dfout1, dfout2), lat.lon.scaling)

	pop.ratio.v <- population.v / (population.v + population.n)

	paid.by.v <- fixed.municipality.cost * pop.ratio.v
	surplus.v <- income.per.capita * population.v - paid.by.v
	distance.merged.v <- get.value(dfout1, med[1], med[2])

	paid.by.n <- fixed.municipality.cost - paid.by.v
	surplus.n <- income.per.capita * population.n - paid.by.n
	distance.merged.n <- get.value(dfout2, med[1], med[2])

	v.util.t <- vector(length=4)
	v.t1 <- V(g)[v]$type.1
	v.t2 <- V(g)[v]$type.2
	v.t3 <- V(g)[v]$type.3
	v.t4 <- V(g)[v]$type.4
	v.util.t[1] <- (3 * v.t1 - v.t2 - v.t3 - v.t4) * v.t1
	v.util.t[2] <- (3 * v.t2 - v.t1 - v.t3 - v.t4) * v.t2
	v.util.t[3] <- (3 * v.t3 - v.t1 - v.t2 - v.t4) * v.t3
	v.util.t[4] <- (3 * v.t4 - v.t1 - v.t2 - v.t3) * v.t4
	v.type.util <- sum(v.util.t) * population.factor

	n.util.t <- vector(length=4)
	n.t1 <- V(g)[n]$type.1
	n.t2 <- V(g)[n]$type.2
	n.t3 <- V(g)[n]$type.3
	n.t4 <- V(g)[n]$type.4
	n.util.t[1] <- (3 * n.t1 - n.t2 - n.t3 - n.t4) * n.t1
	n.util.t[2] <- (3 * n.t2 - n.t1 - n.t3 - n.t4) * n.t2
	n.util.t[3] <- (3 * n.t3 - n.t1 - n.t2 - n.t4) * n.t3
	n.util.t[4] <- (3 * n.t4 - n.t1 - n.t2 - n.t3) * n.t4
	n.type.util <- sum(n.util.t) * population.factor

	utility.v <- surplus.v - distance.merged.v * hatred.ratio + v.type.util
	utility.n <- surplus.n - distance.merged.n * hatred.ratio + n.type.util

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
			for (n in random.neighborhood(g, v, improvable)) {
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
			membership="concat", utility="first", x="first", y="first", type.1="sum",
			type.2="sum", type.3="sum", type.4="sum", dead="first", ocean="first",
			"ignore"))
		
		for (v in V(g)[degree(g) > 0])
			V(g)[v]$utility <- utility.function(g, v, cls, hatred.ratio, lat.lon.scaling)

		if (!changed)
			break
	}
	return(g)
}

merger.utility.2 <- function(g, v, n, cls, hatred.ratio, lat.lon.scaling) {
	population <- sum(V(g)[c(v, n)]$population)
	if (population == 0)
		return(0)
	members <- unlist(V(g)[c(v, n)]$membership)
	surplus <- population * income.per.capita - fixed.municipality.cost

	dfout <- cls[[members[1]]]
	for (i in members[-1])
		dfout <- rbind(dfout, cls[[i]])


	med <- get.med(dfout, lat.lon.scaling)

	distance.merged <- get.value(dfout, med[1], med[2])

	util.t <- vector(length=4)
	t1 <- sum(V(g)[c(v, n)]$type.1)
	t2 <- sum(V(g)[c(v, n)]$type.2)
	t3 <- sum(V(g)[c(v, n)]$type.3)
	t4 <- sum(V(g)[c(v, n)]$type.4)
	util.t[1] <- (3 * t1 - t2 - t3 - t4) * t1
	util.t[2] <- (3 * t2 - t1 - t3 - t4) * t2
	util.t[3] <- (3 * t3 - t1 - t2 - t4) * t3
	util.t[4] <- (3 * t4 - t1 - t2 - t3) * t4
	type.util <- sum(util.t) * population.factor

	return(surplus - distance.merged * hatred.ratio + type.util)
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
			for (n in random.neighborhood(g, v, improvable)) {
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
			membership="concat", utility="first", x="first", y="first", type.1="sum",
			type.2="sum", type.3="sum", type.4="sum", ocean="first", dead="first",
			"ignore"))
		
		for (v in V(g)[degree(g) > 0])
			V(g)[v]$utility <- utility.function(g, v, cls, hatred.ratio, lat.lon.scaling)

		if (!changed)
			break
	}
	return(g)
}

generate.prefecture <- function(n) {
	if (type == "national") {
		center <- get.center()
	} else {
		center <- get.center.2()
	}

	mean.lat <- center[2]
	lat.lon.scaling <- cos(to.radians(mean.lat))

	points <- simplex_poisson(height, width, new.points.count,
		points.octaves, points.scale, points.persistence, points.min,
		points.max, runif(1, 0, 100))

	x <- (points$x - width / 2) / lat.lon.scaling + center[1]
	y <- points$y - height / 2 + center[2]
	d <- deldir(x, y)
	dead.indices <- get.dead.indices(d, x, y)
	g <- get.delaunay.graph(d, x, y, dead.indices)
	g <- generate.coastline(g, n, center, lat.lon.scaling)
	V(g)[V(g)[ocean]]$dead <- TRUE
	V(g)[V(g)[dead]]$ocean <- TRUE
	g[V(g)[ocean], ] <- FALSE

	spatial.pgons <- get.spatial.polygons(d, length(x), dead.indices, center, lat.lon.scaling)
	pop.out <- extract(mesh.r, spatial.pgons, small=TRUE, weights=TRUE, cellnumbers=TRUE)
	cls <- lapply(pop.out, get.pop.df)

	for (v in V(g)[!ocean & !dead]) {
		V(g)[v]$population <- floor(sum(cls[[v]][, "adj.pop"]))
		types <- floor(runif(V(g)[v]$population, 1, 5))
		V(g)[v]$type.1 <- min(length(types[types == 1]), V(g)[v]$population)
		V(g)[v]$type.2 <- min(length(types[types == 2]), V(g)[v]$population)
		V(g)[v]$type.3 <- min(length(types[types == 3]), V(g)[v]$population)
		V(g)[v]$type.4 <- min(length(types[types == 4]), V(g)[v]$population)
	}

	for (v in V(g)[!dead & !ocean])
		V(g)[v]$utility <- utility.function(g, v, cls, hatred.ratio, lat.lon.scaling)

	merged.dem <- merge.1(g, lat.lon.scaling, cls, hatred.ratio)
	merged.dict <- merge.2(g, lat.lon.scaling, cls, hatred.ratio)

	mapping.dem <- 1:vcount(g)
	for (v in V(merged.dem))
		mapping.dem[unlist(V(merged.dem)[v]$membership)] <- v

	mapping.dict <- 1:vcount(g)
	for (v in V(merged.dict))
		mapping.dict[unlist(V(merged.dict)[v]$membership)] <- v

	output <- list()
	output[["premerge"]] <- list(g, spatial.pgons)
	output[["gdem"]] <- list(merged.dem, unionSpatialPolygons(spatial.pgons, mapping.dem, avoidUnaryUnion=TRUE))
	output[["gdict"]] <- list(merged.dict, unionSpatialPolygons(spatial.pgons, mapping.dict, avoidUnaryUnion=TRUE))
	return(output)
}
