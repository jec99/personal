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

bool prime(int p) {
	bool prime = true;
	if (p % 2 == 0 || p < 2) return false;
	for (int i = 2; i < p; ++i)
		if (p % i == 0) prime = false;
	return prime;
}

int quadprimes(int a, int b) {
	int primes = 0;
	for (int n = 0; true; ++n)
		if (prime(n * n + a * n + b))
			primes++;
		else
			break;
	return primes;
}

int main(int argc, char const *argv[]) {
	int t, maxa, maxb, max = 0;
	for (int b = 2; b < 1000; ++b)
		for (int a = -b; a < 1000; ++a)
			if ((t = quadprimes(a, b)) > max) {
				max = t;
				maxa = a;
				maxb = b;
			}

	printf("(a = %d, b = %d, a * b = %d)\n", maxa, maxb, maxa * maxb);
	return 0;
}