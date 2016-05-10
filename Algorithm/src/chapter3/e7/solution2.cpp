#include <stdio.h>

void f(int n)
{
    if (n < 10) printf("%d", n);
    else
    {
        printf("%d", n % 10);
        f(n / 10);
    }
}

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);
    f(n);
    return 0;
}
