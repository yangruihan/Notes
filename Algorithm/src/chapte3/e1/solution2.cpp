#include <stdio.h>

/**
 * 在 solution1 上进行优化，阶乘不需要全部算完，并且正负标记也不需要循环求得
 **/
int main(int argc, char const *argv[])
{
    int n, sign;
    float s, t = 1;

    scanf("%d", &n);
    s = 1;
    sign = 1;
    for (int i = 2; i <= n; ++i)
    {
        sign *= -1;
        t *= (2 * i - 2) * (2 * i - 1);
        s += sign / t;
    }

    printf("%.10f\n", s);
    return 0;
}