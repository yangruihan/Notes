#include <stdio.h>

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);

    int i = 1; // 计数变量
    int dir = 1; // 记录方向变量 1 向下 2 向右 3 向上 4 向左
    int x = 0, y = 0; // 记录当前位置
    int a[100][100];

    // 初始化数组元素为0
    for (int i = 0; i < 100; ++i)
        for (int j = 0; j < 100; ++j)
            a[i][j] = 0;


    // 当i小于n²时，即说明数字还未填满
    while (i <= n * n)
    {
        switch(dir) 
        {
            case 1: 
            {
                a[x++][y] = i++;
                // 如果已经向下到头了，则要转向了
                if(x >= n || a[x][y] != 0) 
                {
                    x--;
                    y++;
                    dir = 2;
                }
                break;
            }
            case 2:
            {
                a[x][y++] = i++;
                // 如果已经向右到头了，则要转向了
                if (y >= n || a[x][y] != 0)
                {
                    y--;
                    x--;
                    dir = 3;
                }
                break;
            }
            case 3:
            {
                a[x--][y] = i++;
                // 如果已经向上到头了，则要转向了
                if (x < 0 || a[x][y] != 0)
                {
                    x++;
                    y--;
                    dir = 4;
                }
                break;
            }
            case 4:
            {
                a[x][y--] = i++;
                // 如果已经向左到头了，则要转向了
                if (y < 0 || a[x][y] != 0)
                {
                    y++;
                    x++;
                    dir = 1;
                }
                break;
            }
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
