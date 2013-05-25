#include <stdlib.h>
#include <iostream>
#include <ios>
#include <cstdlib>
#include <memory>
#include <iterator>
#include <algorithm>
#include <set>
#include <vector>

std::vector<int> digits(int n) {
	std::vector<int> digs;

	while (n != 0) {
		digs.push_back(n % 10);
		n = n / 10;
	}

	return digs;
}

bool panprod(int x, int y) {
	std::vector<int> dx = digits(x);
	std::vector<int> dy = digits(y);
	std::vector<int> dz = digits(x * y);

	for (std::vector<int>::iterator i = dx.begin(); i != dx.end(); ++i)
		dz.push_back(*i);
	for (std::vector<int>::iterator i = dy.begin(); i != dy.end(); ++i)
		dz.push_back(*i);

	bool all = true;
	for (int i = 1; i <= 9; ++i)
		all = all && (std::find(dz.begin(), dz.end(), i) != dz.end());

	return dz.size() == 9 && all;
}

int main(int argc, char const *argv[]) {
	std::set<int> s;
	int sum = 0;
	for (int x = 1; x < 10000; ++x)
		for (int y = 1; y < 10000; ++y)
			if (panprod(x, y))
				s.insert(x * y);

	for (std::set<int>::iterator i = s.begin(); i != s.end(); ++i)
		sum += *i;

	printf("%d\n", sum);
	return 0;
}