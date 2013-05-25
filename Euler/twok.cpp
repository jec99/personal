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
	int digits[1000] = { 1 };
	int sum = 0, carry = 0;

	for (int i = 0; i < 1000; ++i) {
		for (int j = 0; j < 1000; ++j) {
			sum = 2 * digits[j] + carry;
			digits[j] = sum % 10;
			carry = sum / 10;
		}
	}

	sum = 0;
	for (int i = 0; i < 1000; ++i)
		sum += digits[i];

	printf("%d\n", sum);

	return 0;
}