#include <stdio.h>

int main(int argc, char* argv[])
{
    int a[101], n;
    // 初始化数组
    for (int i = 0; i < 101; ++i)
        a[i] = 0;

    scanf("%d", &n);

    for (int i = 2; i <= n; ++i) 
        for (int k = 1; k * i <= n; k++)
            a[k * i] = 1 - a[k * i];

    for (int i = 1; i <= n; ++i)
        if (a[i] == 1) printf("%d ", i);

    return 0;
}
