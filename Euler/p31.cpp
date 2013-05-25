#include <stdlib.h>
#include <iostream>
#include <ios>
#include <cstdlib>
#include <memory>

using namespace std;

int denoms[8] = { 1, 2, 5, 10, 20, 50, 100, 200 };

int main(int argc, char const *argv[]) {
	int combs[200 + 1] = { 1 };

	for (int i = 0; i < 8; ++i)
		for (int j = denoms[i]; j <= 200; ++j)
			combs[j] += combs[j - denoms[i]];

	printf("%d\n", combs[200]);
	return 0;
}