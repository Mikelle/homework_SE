#ifndef LIST_H
#define LIST_H

struct List {
    int data;
    struct List* next;
};

typedef struct List* list;

void list_addElm(list *, int);

void list_delElm(list *, int);

void list_reverse(list *, list);

void list_print(list);

void list_delList(list *);

#endif