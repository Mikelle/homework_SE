/* 
   Linked List
   Author: Mikhail Wall
*/

#include <stdio.h>
#include <stdlib.h>
#include "List.h"

void list_addElm(list *pointer, int data)
{
	if (*pointer == NULL)
	{
		*pointer = (list)malloc(sizeof(struct List));
		(*pointer)->data = data;
		(*pointer)->next = NULL;
        }
        else 
	{
		list temp = (list)malloc(sizeof(struct List));
		temp->data = data;
		temp->next = *pointer;
		*pointer = temp;
        }
}

void list_delElm(list *pointer, int data) 
{
	if ((*pointer)->data == data) 
	{
		list temp = *pointer;
                *pointer = (*pointer)->next;
		free(temp);
        }
        else
	{
		list head = *pointer;
		while ((*pointer)->next->data != data) 
		{
			(*pointer) = (*pointer)->next;
			if ((*pointer)->next == NULL)
			{
                        	printf("There is no such number\n");
				*pointer = head;
                		return;
            		}
        	}
		list temp = (*pointer)->next;
		(*pointer)->next = (*pointer)->next->next;
		free(temp);
		*pointer = head;
    	}
	if (*pointer == NULL) 
	{
        	printf("Sorry, I cannot delete nothing :(\n");
    	}
}

void list_print(list pointer)
{
	if (pointer == NULL) 
	{
        	printf("There is nothing in this list\n");
    	}
    	else 
	{
		while (pointer != NULL)
		{
			printf("%d", pointer->data);
			pointer = pointer->next;
        	}
        	printf("\n");
    	}
}

void list_reverse(list *res, list target)
{
    while (target != NULL) 
	{
        	list_addElm(res, target->data);
        	target = target->next;
    	}	
}

void list_delList(list *pointer) 
{
	if (*pointer == NULL) 
	{
        	printf("Sorry, there is no list to delete :(\n");
    	}
    	else 
	{
		list del = *pointer;
		while ((*pointer) != NULL)
		{
			del = *pointer;
			*pointer = (*pointer)->next;
			free(del);
        	}
    	} 
}
