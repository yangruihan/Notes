#include <stdio.h>

#define MAX_NUM 100

int a[MAX_NUM];

/**
 * 判断一个数是不是完数
 * 返回值：
 *      如果这个数不是完数，则返回0
 *      如果这个数是完数，则返回这个数因子的个数
 **/
int is_perfect_num(int n)
{
    int k = 0;
    int sum = 0;
    for(int i = 1; i < n; ++i)
        if (n % i == 0) 
        {
            sum += i;
            a[k++] = i; 
        }
    if (sum == n) return k - 1;
    else return 0; 
}

int main(int argc, char const *argv[])
{
    int k;
    for(int i = 2; i <= 1000; ++i)
        if ((k = is_perfect_num(i)) > 0) 
        {
            printf("%d it's factors are ", i);
            for (int j = 0; j < k; ++j)
                printf("%d,", a[j]);
            printf("%d\n", a[k]);
        }
    return 0;
}