#include <stdlib.h>
#include <iostream>
#include <ios>
#include <fstream>
#include <algorithm>
#include <vector>
#include <memory>
#include <cstdlib>

using namespace std;

int main(int argc, char const *argv[]) {
	int a[100][50];
	int i = 0, j = 0, c;
	ifstream num;
	num.open("number.txt");

	while (num.good()) {
		c = num.get();
		if (c == '\n') {
			i++;
			j = 0;
		} else {
			a[i][j++] = c - 48;
		}
	}

	num.close();

	int carry = 0;
	int rem[50] = { 0 };

	for (int i = 0; i != 50; ++i) {
		for (int j = 0; j != 100; ++j) {
			carry += a[j][i];
		}
		rem[i] = carry % 10;
		carry /= 10;
	}

	printf("%d", carry);
	for (int i = 0; i < 50; ++i)
		cout << rem[50 - i - 1];

	cout << endl;

	return 0;
}