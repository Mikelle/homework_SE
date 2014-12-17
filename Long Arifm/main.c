#include <stdio.h>
#include "List.h"
#include "LongArifm.h"

int main()
{
	longNum a = {0};
	longNum b = {0};
    longNum sum= {0};
	longNum_input(&a);
	longNum_input(&b);
	longNum_sum(&sum, a, b);
	longNum_showNum(sum);

    return 0;
}