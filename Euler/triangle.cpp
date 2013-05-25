#include <stdlib.h>
#include <iostream>
#include <ios>
#include <fstream>
#include <algorithm>
#include <vector>
#include <memory>
#include <cstdlib>

#define n (100)

using namespace std;

int main(int argc, char const *argv[]) {
	int c, current = 0, i = 0, j = 0;
	int a[n][n] = { 0 };
	ifstream triangle;
	triangle.open("triangle.txt");

	while ((c = triangle.get()) != EOF)
		if (c == '\n') {
			a[i][j] = current;
			current = j = 0;
			i++;
			continue;
		} else if (c == ' ') {
			a[i][j++] = current;
			current = 0;
		} else if (isdigit(c)) {
			current = 10 * current + (c - 48);
		}

	a[n - 1][n - 1] = current;

	int p[n][n] = { 0 };
	p[0][0] = a[0][0];
	for (i = 1; i != n; ++i)
		for (j = 0; j != i + 1; ++j)
			if (j == 0)
				p[i][j] = p[i - 1][j] + a[i][j];
			else if (j == i)
				p[i][j] = p[i - 1][j - 1] + a[i][j];
			else
				p[i][j] = max(p[i - 1][j], p[i - 1][j - 1]) + a[i][j];

	int maximum = 0;
	for (i = 0; i < n; ++i)
		if (maximum < p[n - 1][i]) maximum = p[n - 1][i];

	printf("%d \n", maximum);

	return 0;
}