#include <stdio.h>

int Divinteger(int n, int m)
{
    // 当m>n时，是没有意义的，因为不存在一种划分使得划分出来的因子还要比最终的和大
    if(m > n) return Divinteger(n, n);

    // 当n或者m等于1时，此时都只有一种划分
    // n等于1时表示，被划分数为1，显然只有1这种划分
    // m等于1时表示，将被划分数划分成若干个1相加，显然也只有1+1+...+1这种划分
    else if(n == 1 or m == 1) return 1;

    // 当n和m相等时，则总划分数为本身的1加上将n划分成不超过本身-1的总划分数
    else if(n == m) return 1 + Divinteger(n, m - 1);

    // 剩下的划分数等于不包含m的划分数加上包含m的划分数
    else return Divinteger(n, m - 1) + Divinteger(n - m, m);
}

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);
    printf("%d", Divinteger(n, n));
    return 0;
}
