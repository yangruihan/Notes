#include <stdio.h>

int main(int argc, char const *argv[])
{
    int sign, n;
    float s = 1, t;

    scanf("%d", &n);
    // 循环累加
    for(int i = 2; i <= n; i++)
    {
        // 求阶乘
        t = 1;
        for(int j = 2; j <= 2 * i - 1; j++) t *= j;

        // 求符号
        sign = 1;
        for(int j = 1; j <= i + 1; j++) sign *= (-1);

        // 累加
        s += sign / t;
    }

    printf("%.10f\n", s);

    return 0;
}