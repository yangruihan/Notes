#include <stdio.h>

int hanoi(char from, char to, char mid, int num)
{
    if(num == 1)
    {
        printf("%c->%c\n", from, to);
        return 1;
    }
    else return hanoi(from, mid, to, num - 1) + hanoi(from, to, mid, 1) + hanoi(mid, to, from, num - 1);
}

int main(int argc, char const *argv[])
{
    int n;
    scanf("%d", &n);
    printf("%d\n", hanoi('A', 'B', 'C', n));
    return 0;
}