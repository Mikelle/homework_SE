/*
	Buffer Overflow
	Author: Mikhail Wall
*/

#include <stdio.h>
#include <string.h>

void roots()
{
    /* Now Give root or admin rights to user*/
    printf ("\n Root privileges given to the user \n");
}

int checked(char password[])
{
    int pass = 0;
    char buffer[10];
    strcpy(buffer, password);
    if !(strcmp(buffer, "password")) pass = 1;
    return pass;
}

int main(void)
{
    char string[50];

    printf("\n Enter the password : \n");
    gets(string);

    if (checked(string)) roots();
    else printf("\n Wrong Password \n");
    
    return 0;
}
