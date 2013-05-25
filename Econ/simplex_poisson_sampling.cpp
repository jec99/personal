#include <iostream>
#include <ios>

#include <vector>
#include <algorithm>
#include <list>
#include <cctype>
#include <cstdlib>
#include <iterator>
#include <numeric>
#include <cstddef>
#include <memory>
#include <math.h>

#include "mtrand.h"

#define PI (3.1415926535)

int fastfloor(const double x);
double dot(const int *g, const double x, const double y, const double z);

unsigned long init[4] = { 0x123, 0x234, 0x345, 0x456 }, length = 4;
MTRand_int32 irand(init, length);
MTRand drand;

double random_real(double low, double high) {
  double diff = high - low;
  double ret = drand();
  return ret * diff + low;
}

double random_real(double high) {
  return random_real(0.0, high);
}

int random_int(int n) {
  return (int) fastfloor(random_real(0.0, n));
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

class Point {
public:
  double x, y;

  Point(double why, double ecks): x(ecks), y(why) { }
  Point(): x(-1.0), y(-1.0) { }

  Point operator*(Point p) const {
    return Point(x * p.x, y * p.y);
  }

  Point operator*(double d) const {
    return Point(x * d, y * d);
  }

  Point operator+(Point p) const {
    return Point(x + p.x, y + p.y);
  }

  Point operator-(Point p) const {
    return Point(x - p.x, y - p.y);
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

  Point first(random_real(height), random_real(width));
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

int main(int argc, char **argv) {
  double ht = atof(argv[1]);  // height
  double wd = atof(argv[2]);  // width
  int np = atoi(argv[3]);     // new points count

  double a = atof(argv[4]);   // octaves
  double b = atof(argv[5]);   // persistence
  double c = atof(argv[6]);   // scale
  double d = atof(argv[7]);   // min
  double e = atof(argv[8]);   // max
  double f = atof(argv[9]);   // seed

  std::vector<Point> pts = poisson_sample(ht, wd, np, a, b, c, d, e, f);
  for (int i = 0; i < pts.size(); ++i) {
    std::cout << pts[i].x << ", " << pts[i].y << std::endl;
  }
  return 0;
}
