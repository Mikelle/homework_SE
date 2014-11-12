/*
	Float representation
	Author: Mikhail Wall
*/

#include <stdio.h>
#define max 255

int sign = 0, exp = 0, mant = 0;

void number(int bits)
{
    sign = (bits >> 31) & 1;
    exp = (bits >> 23) & ((1 << 8) - 1);
    mant = bits & ((1 << 23) - 1);
}

void f1() //pointers
{
    float num;
    scanf("%f", &num);
    number(*(int *)(& num));
}

void f2() //union
{
    union
    {
        float fval;
        int ival;
    } fnum;
    
    scanf("%f", &fnum.ival);
    number(fnum.ival);
}

void f3() //bit field
{
    union
    {
        float fval;
        struct
        {
            unsigned m : 23;
            unsigned e : 8;
            unsigned s : 1;
        } bitField;
    } fnum;
    
    scanf("%f", &fnum.fval);
    sign = fnum.bitField.s;
    exp = fnum.bitField.e;
    mant = fnum.bitField.m;
}


int main()
{
    f1(); 
    //f2(); 
    //f3(); 
     
    if (sign) sign = -1; else sign = 1;
    if (exp == 0 && mant == 0) printf("0\n");
	else if (exp == max && mant != 0) printf("NaN\n");
    else if (exp == max && mant == 0) 
    {
        if (sign > 0) printf("positive infinity\n");
        else printf("negative infinity\n");
    }
    else printf("%d * 2^%d * %f\n", sign, exp - 127, 1 +((float)mant) / (1 << 23));
    return 0;
}