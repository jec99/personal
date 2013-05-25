#include <stdlib.h>
#include <iostream>
#include <algorithm>
#include <vector>
#include <memory>

using namespace std;

int main(int argc, char const *argv[]) {
	int n = atoi(argv[1]);
	long int sum = 0;
	int *a = (int *) malloc(sizeof(int) * (n + 1));

	for (int i = 0; i < n; ++i)
		a[i] = i;

	for (int i = 2; i < n; ++i) {
		if (a[i] == 0) continue;
		sum += (long int) a[i];
		for (int j = 2 * i; j < n; j += i) {
			if (j >= n) break;
			a[j] = 0;
		}
	}

	cout << sum << endl;

	free(a);

	return 0;
}
