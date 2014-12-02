/*
	Find substring in string
	Author: Mikhail Wall
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
 
int main() {
	int i, j, amount, k, lenString, lenSubString;
	char string[100], subString[100];
	printf("Input string\n");
	scanf("%s", string);
	printf("Input subString\n");
	scanf("%s", subString);
	lenString = strlen(string);
	lenSubString = strlen(subString);
	amount = 0;
	for (i = 0; i <= lenString; i++)
	{
		string[i] = tolower (string[i]);
	}
	for (j = 0; j <= (lenString - lenSubString); j++)
	{
		k = 0;
		for (i = 0; i < lenSubString; i++)
		{
			if (string[j + i] != subString[i])
			{
				k = 1;
			}
		}
		if (k == 0)
		{
			amount++;
		}
	}
	printf("Answer: %d\n", amount);
}