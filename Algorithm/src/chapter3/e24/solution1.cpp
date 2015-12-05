#include <stdio.h>

int main(int argc, char* argv[])
{
    int n, a[100][3];
    // 初始化数组元素为0
    for (int i = 0; i < 100; ++i)
        for (int j = 0; j < 3; ++j)
            a[i][j] = 0;

    scanf("%d", &n);
    for (int i = 0; i < n; ++i)
        for (int j = 0; j < 3; ++j)
            // 1 表示红花 2 表示黄花 3 表示蓝花
            scanf("%d", &a[i][j]);

    int m; // 用来记录交换次数
    // 找只需要对调交换的花盆
    for (int i = 0; i < n; ++i)
    {
        // 本应该放红花的地方放置的是黄花
        if (a[i][1] == 2)
            // 遍历寻找本应该放黄花的地方放的是红花的花盆
            for (int j = 0; j < n; ++j)
                // 找到则进行交换
                if (a[j][2] == 1)
                {
                    int t = a[i][1];
                    a[i][1] = a[j][2];
                    a[j][2] = t;
                    m += 3; // 进行一次交换需要3步
                    break;
                }
        // 同理，本应该是放红花的地方放置的是蓝花
        if (a[i][1] == 3)
            for (int j = 0; j < n; ++j)
                if (a[j][3] == 1)
                {
                    int t = a[i][1];
                    a[i][1] = a[j][3];
                    a[j][3] = t;
                    m += 3;
                    break;
                }
        // 同理，本应该是放黄花的地方放置的是蓝花
        if (a[i][2] == 3)
            for (int j = 0; j < n; ++j)
                if (a[j][3] == 2)
                {
                    int t = a[i][2];
                    a[i][2] = a[j][3];
                    a[j][3] = t;
                    m += 3;
                    break;
                }
    }

    // 找需要循环交换的花盆
    for (int i = 0; i < n; ++i)
    {
        // 本应该放红花的地方放置的黄花
        if (a[i][1] == 2)
            for (int j = 0; j < n; ++j)
                // 本应该放黄花的地方放置的蓝花
                if (a[j][2] == 3)
                    for (int k = 0; k < n; ++k)
                        // 本应该放蓝花的地方放的红花
                        if (a[k][3] == 1)
                        {
                            int t = a[i][1];
                            a[i][1] = a[j][2];
                            a[j][2] = a[k][3];
                            a[k][3] = t;
                            m += 4;
                            break;
                        }
        // 同理还有一种情况
        if (a[i][1] == 3)
            for (int j = 0; j < n; ++j)
                if (a[j][2] == 1)
                    for (int k = 0; k < n; ++k)
                        if (a[k][3] == 2)
                        {
                            int t = a[i][1];
                            a[i][1] = a[j][2];
                            a[j][2] = a[k][3];
                            a[k][3] = t;
                            m += 4;
                            break;
                        }
    }

    printf("总共需要交换的次数: %d\n", m);

    return 0;
}
