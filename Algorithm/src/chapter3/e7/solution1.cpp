#include <stdio.h>

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);
    int m = n;
    while(m != 0)
    {
        printf("%d", m % 10);
        m /= 10;
    }
    return 0;
}
