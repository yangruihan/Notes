#include <stdio.h>

int main(int argc, char* argv[])
{
    int n; // n个小朋友
    scanf("%d", &n);
    int a[n]; // 用一维数组表示有n个小朋友，0表示在圈内，1表示在圈外
    for (int j = 0; j < n; ++j)
        a[j] = 0;
    int m; // 报到m的出队
    int start; // 从第start个位置的小朋友开始报数
    scanf("%d%d", &m, &start);
    start--; // 因为数组是从0开始，因此这里要减一
    int num = 1; // 报数
    int old; // 上一个在圈中人的位置，用于最后输出结果

    // 当前还有i个小朋友在圈内
    for(int i = n; i > 1; start++)
    {
        // 当前的小朋友还在圈内
        if(a[start %= n] == 0)
            // 如果该小朋友报到m，则出队，报数从1开始，圈内人数减一
            if (num == m)
            {
                a[start] = 1;
                num = 1;
                i--;
            }
            else 
            {
                num++;
                old = start;
            }
    }

    // 数组是从0开始，因此这里输出要加一
    printf("%d\n", old + 1);

    return 0;
}
