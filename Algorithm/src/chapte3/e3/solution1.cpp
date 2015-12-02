#include <stdio.h>

#define MAX_NUM 100

int a[MAX_NUM][MAX_NUM];
int n;

/**
 * 找到第 r 行最小值的列数
 */
int find_min(int r)
{
    int min = a[r][0];
    int c = 0;
    for (int i = 1; i < n; ++i)
        if (min > a[r][i])
        {
            min = a[r][i];
            c = i;
        }
    return c;
}

/**
 * 找到第 c 列最大值的行数
 */
int find_max(int c)
{
    int max = a[0][c];
    int r = 0;
    for (int i = 1; i < n; ++i)
        if (max < a[i][c])
        {
            max = a[i][c];
            r = i;
        }
    return r;
}

int main(int argc, char const *argv[])
{
    scanf("%d", &n);
    for(int i = 0; i < n; ++i)
        for (int j = 0; j < n; ++j)
            scanf("%d", &a[i][j]);

    bool have_result = false;
    int c, r;
    for(int i = 0; i < n; ++i)
    {
        // 找到第 i 行的最小值所在的列 c
        c = find_min(i);
        // 找到第 c 列的最大值所在的行 r
        r = find_max(c);
        // 如果所在的行和当前行相等，说明是鞍点
        if(r == i)
        {
            printf("The result is [%d, %d]\n", c, r);
            have_result = true;
        }
    }
    // 如果没有解
    if (!have_result)
        printf("There is no result!\n");

    return 0;
}