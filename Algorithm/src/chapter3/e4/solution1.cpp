#include <stdio.h>

int main(int argc, char const *argv[])
{
    int a[100][100], n, k;
    scanf("%d", &n);
    k = 1;
    for(int i = 1; i <= n; i++)
        for(int j = 1; j <= n + 1 - i; j++)
        {
            a[i - 1 + j][j] = k;
            k++;
        }
    for(int i = 1; i <=n; i++)
    {
        for(int j = 1; j<=i; j++)
            printf("%d\t", a[i][j]);
        printf("\n");
    }
    return 0;
}