#include <stdio.h>

int a[100];

/**
 * 从n个数中选出m个
 */
void f(int m, int k)
{
    for (int i = m; i >= k; --i)
    {
        a[k] = i;
        if(k > 1) f(i - 1, k - 1);
        else
        {
            for(int j = a[0]; j > 0; --j)
                printf("%d\t", a[j]);
            printf("\n");
        }
    }
}

int main(int argc, char* argv[])
{
    int n, r;
    scanf("%d%d", &n, &r);
    if(r > n)
        printf("Input Error");
    else
    {
        a[0] = r;
        f(n, r);
    }
    return 0;
}
