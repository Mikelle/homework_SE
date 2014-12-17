#include <stdio.h>
#include "LongArifm.h"
#include "List.h"

void longNum_input(longNum *result)
{
    result-> digits = NULL;
    result-> sign = 0;
    result-> numSize = 0;
    result-> isCalculated = 0;
    char c = ' ';
	list temp = NULL;

    while(1) 
	{
        scanf("%c", &c);
        if (c == '-')
		{
            !!(result->sign) ? (result->sign = 0) : (result->sign = -1);
        }
        if ((c >= 48) && (c <= 57)) 
		{
            list_addElm(&temp, (int)(c - '0'));
            result->numSize++;
        }
        else if (c == 'q')
		{
            break;
        }
    }
    result->digits = temp;
}

int longNum_less(longNum first, longNum second) 
{
    if (first.numSize > second.numSize) 
	{
        return 2;
    }

    else if(first.numSize < second.numSize) 
	{
        return 1;
    }

    else
	{
		list rev1 = NULL,
             rev2 = NULL;
        list_reverse(&rev1, first.digits);
        list_reverse(&rev2, second.digits);

        while ((rev1->data == rev2->data) && (rev1->next != NULL)) 
		{
            rev1 = rev1->next;
            rev2 = rev2->next;
        }
        if (rev1->data < rev2->data)
		{
            list_delList(&rev1);
            list_delList(&rev2);
            return 1;
        }
        else if (rev1->data > rev2->data)
		{
            list_delList(&rev1);
            list_delList(&rev2);
            return 2;
        }
        else
		{
            list_delList(&rev1);
            list_delList(&rev2);
            return 0;
        }
    }
}

void longNum_sum(longNum *res, longNum first, longNum second) 
{
    res->isCalculated = 1;
    res->digits = NULL;
    int dec = 0;

	if (first.sign == second.sign)
	{
		res->sign = first.sign;
		while ((first.digits != NULL) && (second.digits != NULL)) 
		{
			list_addElm(&(res->digits), (first.digits->data + second.digits->data + dec) % 10);
			dec = (first.digits->data + second.digits->data + dec) / 10;
			first.digits = first.digits->next;
			second.digits = second.digits->next;
        }

		if (first.numSize > second.numSize)
		{
            int i = 0;
			for (i; i < first.numSize - second.numSize; i++) 
			{
				list_addElm(&(res->digits), (first.digits->data + dec) % 10);
				dec = (first.digits->data + dec) / 10;
				first.digits = first.digits->next;
            }
        }
		else if (first.numSize < second.numSize)
		{
            int i = 0;
			for (i; i < second.numSize - first.numSize; i++) 
			{
				list_addElm(&(res->digits), (second.digits->data + dec) % 10);
				dec = (second.digits->data + dec) / 10;
				second.digits = second.digits->next;
            }
        }
        if (dec == 1)
		{
            list_addElm(&(res->digits), 1);
        }
    }
    else
	{
		if (longNum_less(first, second) == 1)
		{
			longNum_subtr(res, second, first);
			res->sign = second.sign;
        }
		else if (longNum_less(first, second) == 2) 
		{
			longNum_subtr(res, first, second);
			res->sign = first.sign;
        }
        else
		{
            list_addElm(&(res->digits), 0);
        }
    }
}

void longNum_subtr(longNum *res, longNum bigger, longNum less)
{
    res->digits = NULL;
    while ((bigger.digits != NULL) && (less.digits != NULL))
	{
        if (bigger.digits->data >= less.digits->data)
		{
            list_addElm(&(res->digits), bigger.digits->data - less.digits->data);
        }
        else
		{
            list_addElm(&(res->digits), (bigger.digits->data + 10) - less.digits->data);
            bigger.digits->next->data--;
        }

        bigger.digits = bigger.digits->next;
        less.digits = less.digits->next;
    }

    if (bigger.numSize > less.numSize) 
	{
        int i = 0;
        for(i; i < bigger.numSize - less.numSize; i++) 
		{
            list_addElm(&(res->digits), bigger.digits->data);
            bigger.digits = bigger.digits->next;
        }
    }
}

void longNum_showNum(longNum target)
{
	list rev = NULL;
    if(target.isCalculated == 0) 
	{
        if(target.sign == 0)
		{
            list_reverse(&rev, target.digits);
            list_print(rev);
        }
        else
		{
            list_reverse(&rev, target.digits);
            printf("-");
            list_print(rev);
        }
    }
    else
	{
        if(target.sign == 0) 
		{
            list_print(target.digits);
        }
        else
		{
            printf("-");
            list_print(target.digits);
        }
    }
}