#include <stdio.h>
#include <cstring>

int main(int argc, char* argv[])
{
    char a[256]; // 乘数
    long b; // 被乘数
    int result[256]; // 结果
    long long mid = 0; // 中间结果
    long long d = 0; // 进位
    int i = 0; // 记录当前结果有多少位

    scanf("%s", a);
    scanf("%ld", &b);
    // 乘数的每一位和被乘数相乘
    for(int j = strlen(a) - 1; j >= 0; --j)
    {
        mid = (a[j] - 48) * b + d;
        result[i++] = mid % 10;
        d = mid / 10;
    }
    // 处理最后一次进位
    while (d != 0)
    {
        result[i++] = d % 10;
        d /= 10;
    }
    // 反向输出结果
    for (int j = i - 1; j >= 0; --j)
        printf("%d", result[j]);
    return 0;
}
