#include <stdio.h>

int main(int argc, char* argv[])
{
    int a[10][10];
    // 初始化数组元素为0
    for (int i = 0; i < 10; ++i)
        for (int j = 0; j < 10; ++j)
            a[i][j] = 0;

    int n, m;
    scanf("%d", &n);

    int a1, a2; // 用来记录相邻两个数字
    scanf("%d", &a1);
    for (int i = 0; i < n - 1; ++i)
    {
        scanf("%d", &a2);
        a[a1][a2]++;
        a1 = a2;
    }

    for (int i = 0; i < 10; ++i)
        for (int j = 0; j < 10; ++j)
            if (a[i][j] && a[j][i]) 
            {
                printf("(%d, %d)=%d  (%d, %d)=%d\n", i, j, a[i][j], j, i, a[j][i]);
                // 将数据破坏，避免重复输出
                a[i][j] = 0;
                a[j][i] = 0;
            }

    return 0;
}
