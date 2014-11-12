/* 
	Linked List
	Author: Mikhail Wall
*/

#include <stdio.h>
#include <stdlib.h>

typedef struct List
{
	int data;
	struct List* next;
};

typedef struct List* point;

void add(point *pointer, int data) {
	if (*pointer == NULL)
	{
		*pointer = (point)malloc(sizeof (struct List));
		(*pointer) -> data = data;
		(*pointer) -> next = NULL;
	}
	else 
	{
		point temp = (point)malloc(sizeof (struct List));
		temp -> data = data;
		temp -> next = *pointer;
		*pointer = temp;
	}
}

void delet(point *pointer, int data)
{
	while ((*pointer) -> next != NULL && ((*pointer) -> next) -> data != data)
	{
		(*pointer) = (*pointer) -> next;
	} 
	if ((*pointer) -> next == NULL)
	{
		printf("Element %d is not present in the List\n", data);
		return;
	}
	point temp;
	temp = (*pointer) -> next;
	(*pointer) -> next = temp -> next;
	free(temp);
	return;
}

void remove(point *pointer, int data) {
	
	if ((*pointer) -> data == data) 
	{
		point temp = *pointer;
		*pointer = (*pointer) -> next;
		free(temp);
	}
	else 
	{
		point head = *pointer;
		while ((*pointer) -> next -> data != data) 
		{
			(*pointer) = (*pointer) -> next;
		}
		point temp = (*pointer) -> next;
		(*pointer) -> next = (*pointer) -> next -> next;
		free(temp);
		*pointer = head;
	}
	if (*pointer == NULL) 
	{
		printf("Element %d is not present in the List\n", data);
		return;
	}
}

void print(point pointer)
{
	if (pointer == NULL)
	{
		return;
	}
	
	printf("%d", pointer -> data);
	printf(", ");
	print(pointer -> next);
}

int main(void) 
{
	point start = NULL;
	char n = 0;

	printf("1. Insert: a\n");
	printf("2. Delete: r\n");
	printf("3. Print: p\n");
	printf("4. Quit: q\n");
	while(n != 'q')
	{
		scanf("%c", &n);
		if (n == 'a')
		{
			int data;
			scanf("%d", &data);
			add(&start, data);
		}
		else if(n == 'r')
		{
			int data;
			scanf("%d", &data);
			remove(&start, data);
		}
		else if(n == 'p')
		{
			printf("The List is ");
			print(start);
			printf("\n");
		}
	}
}