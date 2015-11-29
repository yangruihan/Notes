#include <stdio.h>
#include <cstring>

int main(int argc, char* argv[])
{
    char num[40];
    char eng[10][6] = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};

    scanf("%s", num);
    printf("%s", eng[num[0] - 48]);
    for(int i = 1; i < strlen(num); ++i)
        printf("-%s",eng[num[i] - 48]);

    return 0;
}
