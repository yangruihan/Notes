#include <stdio.h>

int main(int argc, char* argv[])
{
    // 定义一个二维数组，第一维用来存输入的数值，第二维用来存它与相隔一个数据的数的乘积
    int a[100][2], n;
    // 初始化数组
    for (int i = 0; i < 100; ++i)
    {
        a[i][1] = 0;
        a[i][2] = 1;
    }

    scanf("%d", &n);

    for (int i = 0; i < n; ++i)
        scanf("%d", &a[i][1]);

    for (int i = 0; i < n - 2; ++i)
        a[i][2] = a[i][1] * a[i + 2][1];
    a[n - 2][2] = a[n - 2][1] * a[0][1];
    a[n - 1][2] = a[n - 1][1] * a[1][1];

    int max = 0;
    int min = 9999;
    int maxn, minn;
    for (int i = 0; i < n; ++i)
    {
        if (max < a[i][2])
        {
            max = a[i][2];
            maxn = i;
        }
        if (min > a[i][2])
        {
            min = a[i][2];
            minn = i;
        }
    }

    if (maxn == n - 1) 
        printf("max=%d * %d = %d\n", a[n - 1][1], a[1][1], max);
    else if (maxn == n - 2)
        printf("max=%d * %d = %d\n", a[n - 2][1], a[0][1], max);
    else printf("max=%d * %d = %d\n", a[maxn][1], a[maxn + 2][1], max);


    if (minn == n - 1) 
        printf("min=%d * %d = %d\n", a[n - 1][1], a[1][1], min);
    else if (minn == n - 2)
        printf("min=%d * %d = %d\n", a[n - 2][1], a[0][1], min);
    else printf("min=%d * %d = %d\n", a[minn][1], a[minn + 2][1], min);

    return 0;
}
    