#include <stdio.h>
#include <math.h>

int main(int argc, char* argv[])
{
    long a[256]; // 运算结果
    long long b, d; // 中间运算结果和进位
    int m, n, r;

    scanf("%d", &n);

    m = log(n) * n / 6 + 2; // 粗略估计n!结果的位数

    // 对结果数组进行初始化，因为是累乘，因此第一位为1，其余全部置0
    a[1] = 1;
    for(int i = 2; i <=m ; ++i)
        a[i] = 0;

    d = 0;
    // 外层循环控制 n! = (n-1)! * n 中的 n
    for (int i = 2; i <= n; ++i)
    {
        int j;
        // 内层循环控制大整数结果每一位与 n 相乘
        for (j = 1; j <= m; ++j)
        {
            // 中间结果 = 当前一位 * i + 进位
            b = a[j] * i + d;
            // 每一位存储6位数
            a[j] = b % 1000000;
            // 计算下一次的进位
            d = b / 1000000;
        }
        // 如果最后一次还有进位，则放到最后一位
        if (d != 0) a[j] = d;
    }

    // 计算结果一共用了数组多少位
    for (int i = m; i >= 1; --i)
        if (a[i] == 0) continue;
        else { r = i; break; }

    printf("%d!=", n);
    printf("%ld ", a[r]);

    // 循环输出数组的每一位，并且其中不满6位的用0补齐
    for (int i = r - 1; i > 0; --i)
    {
        if (a[i] > 99999) { printf("%ld ", a[i]); continue; }
        if (a[i] > 9999) { printf("0%ld ", a[i]); continue; }
        if (a[i] > 999) { printf("00%ld ", a[i]); continue; }
        if (a[i] > 99) { printf("000%ld ", a[i]); continue; }
        if (a[i] > 9) { printf("0000%ld ", a[i]); continue; }
        printf("00000%d ", a[i]);
    }
    return 0;
}
