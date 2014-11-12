/*
	sign
	Author: Mikhail Wall
*/

#include <cstdio>
#include <iostream>

int sign(int n)
{
	return (n >> 0x1F) + !(n >> 0x1F) + ~(!n + ~0);
}

int main()
{
	int n;
	scanf("%d", &n);
	printf("%d\n", sign(n));

	return 0;
}