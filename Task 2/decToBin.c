/* 
	Decimal to Binary
	Author: Mikhail Wall
*/

#include <iostream>
#include <cstdio>

int main()
{
	
	const int decToBin = sizeof(int) * 8;
	int bitMask = 0x80000000, number = 0, i = 0;
	scanf("%d", &number);
	for (i = 0; i < bitsInInt; ++i)
	{
		printf("%d", number & bitMask ? 1:0);
		bitMask = (bitMask >> 1) & ~bitMask;
	}

	return 0;
}