#include <stdio.h>

int main(int argc, char* argv[])
{
    int a[5] = {0, 0, 0, 0, 0};
    int n;
    scanf("%d", &n);
    while(n != -1)
    {
        if (n > 0 && n <= 5) a[n - 1]++;
        else printf("invalid");
        scanf("%d", &n);
    }
    for(int i = 0; i < 5; i++)
        printf("%d:%d\t", i + 1, a[i]);
    return 0;
}
