#include <stdlib.h>
#include <iostream>
#include <ios>
#include <fstream>
#include <algorithm>
#include <vector>
#include <memory>
#include <cstdlib>
#include <string>
#include <math.h>

using namespace std;

// working
bool prime(unsigned int n) {
	if (n == 2 || n == 3)
		return true;
	else if (n < 2 || n % 2 == 0 || n % 3 == 0)
		return false;
	else {
		for (int i = 1; 6 * i + 1 < sqrt(n); ++i)
			if (n % (6 * i - 1) == 0 || n % (6 * i + 1) == 0)
				return false;
		return true;
	}
}

double spiralprimes(int radius) {
	unsigned int ur[radius], bl[radius], br[radius];

	for (unsigned int k = 0; k < radius; ++k) {
		ur[(int) k] = (2 * (k + 1) + 1) * (2 * (k + 1) + 1) - (2 * (k + 1));
		bl[(int) k] = (2 * (k + 1) + 1) * (2 * (k + 1) + 1) - 2 * (2 * (k + 1));
		br[(int) k] = (2 * (k + 1) + 1) * (2 * (k + 1) + 1) - 3 * (2 * (k + 1));
	}

	double primes = 0;
	for (int k = 0; k < radius; ++k) {
		if (prime(ur[k])) primes++;
		if (prime(bl[k])) primes++;
		if (prime(br[k])) primes++;
	}

	return primes / (4 * ((double) radius) + 1);
}

int main(int argc, char const *argv[]) {
	double d;
	int i = 12800;
	while ((d = spiralprimes(i)) >= 0.1) ++i;
	printf("%d: %f\n", i, spiralprimes(i));
	return 0;
}