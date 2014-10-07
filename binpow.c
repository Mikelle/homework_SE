#include <iostream>
#include <cstdio>

using namespace std;

int binpow(int a, int n) {
	int res = 1;
	while (n) {
		if (n & 1)
			res *= a;
		a *= a;
		n >>= 1;
	}
	return res;
}

int main() {
	int a, n;
	scanf(" %d%d", &a, &n);
	int t = binpow(a, n);
	printf(" %d", t);
}