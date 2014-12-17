#ifndef LONGARIFM_H
#define LONGARIFM_H

#include "List.h"

typedef struct longNumber {
    int sign;
    int numSize;
    int isCalculated;
	list digits;
} longNum;

void longNum_input(longNum *);

int longNum_less(longNum, longNum);

void longNum_sum(longNum *, longNum, longNum);

void longNum_subtr(longNum *, longNum, longNum);

void longNum_showNum(longNum);

#endif