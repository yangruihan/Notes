#include <stdio.h>

int main(int argc, char* argv[])
{
    int a[100][100], n;

    scanf("%d", &n);
    for (int i = 1; i <= n; ++i)
        for (int j = 1; j <= n; ++j)
            if (i == j || i + j == n + 1) a[i][j] = 0;
            else 
                if (i + j < n + 1)
                    if (i < j) a[i][j] = 1;
                    else a[i][j] = 2;
                else 
                    if (i > j) a[i][j] = 3;
                    else a[i][j] = 4;

    for (int i = 1; i <= n; ++i)
    {
        for (int j = 1; j <= n; ++j)            
            printf("%d ", a[i][j]);
        printf("\n");
    }
    return 0;
}
