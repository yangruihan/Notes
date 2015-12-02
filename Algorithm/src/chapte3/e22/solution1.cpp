#include <stdio.h>

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);

    int a[100][100];
    // 初始化数组元素为0
    for (int i = 0; i < 100; ++i)
        for (int j = 0; j < 100; ++j)
            a[i][j] = 0;

    int i = 1; // 记录当前应填写的数字
    int x = 0, y = (n + 1) / 2 - 1; // 记录当前应填写的位置
    int old_x, old_y;
    while (i <= n * n)
    {
        old_x = x;
        old_y = y;
        a[x--][y--] = i++;
        if (x < 0) x = n - 1;
        if (y < 0) y = n - 1;
        if (a[x][y] != 0)
        {
            x = old_x + 1;
            y = old_y;
        }
    }

    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
            printf("%d\t", a[i][j]);
        printf("\n");
    }

    return 0;
}
