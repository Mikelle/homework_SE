/*
	fitbits
	Author: Mikhail Wall
*/

#include <iostream>
#include <cstdio>

int fitsBits(int x, int n) {
	int a, b;
	b = 0x21 + ~n;
	a = !(((x << b) >> b) ^ x);
	return a;
}

int main() {
	int x, n;
	scanf(" %d%d", &x, &n);
	int t = fitsBits(x, n);
	printf(" %d", t);
}