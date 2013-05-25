#include <stdlib.h>
#include <iostream>
#include <ios>
#include <fstream>
#include <algorithm>
#include <vector>
#include <memory>
#include <cstdlib>

using namespace std;

vector<int> factor(int n) {
	vector<int> v;
	int i = 2;
	do {
		if (n % i == 0) {
			n = n / i;
			v.push_back(i);
			i = 2;
		} else {
			i++;
		}
	} while (i != 1);
	return v;
}

vector<int> factors(vector<int> v, n) {
	vector<int> facs(1, n);
	
}

int main(int argc, char const *argv[]) {
	
	return 0;
}