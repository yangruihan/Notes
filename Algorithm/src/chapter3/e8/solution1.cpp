#include <stdio.h>
#include <cstring>

int main(int argc, char* argv[])
{
    char num[100];
    scanf("%s", num);
    for(int i = 0; i < strlen(num); ++i)
        printf("%c", num[i]);
    return 0;
}
