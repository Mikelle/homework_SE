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

typedef struct List* list ;

void add(list *pointer, int data) {
	if (*pointer == NULL)
	{
		*pointer = (point)malloc(sizeof (struct List));
		(*pointer) -> data = data;
		(*pointer) -> next = NULL;
	}
	else 
	{
		list temp = (point)malloc(sizeof (struct List));
		temp -> data = data;
		temp -> next = *pointer;
		*pointer = temp;
	}
}

void remove(list *pointer, int data) {
	
	if ((*pointer) -> data == data) 
	{
		list temp = *pointer;
		*pointer = (*pointer) -> next;
		free(temp);
	}
	else 
	{
		list head = *pointer;
		while ((*pointer) -> next -> data != data) 
		{
			(*pointer) = (*pointer) -> next;
		}
		list temp = (*pointer) -> next;
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

void print(list pointer)
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
	list start = NULL;
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