#include <stdio.h>

int main(int argc, char* argv[])
{
    int money[6] = {50, 20, 10, 5, 2, 1}, a[6];
    int x, y;
    scanf("%d%d", &x, &y);
    y -= x;
    for (int i = 0; i < 6; ++i)
    {
        a[i] = y / money[i];
        y -= a[i] * money[i];
        printf("%d : %d\n", money[i], a[i]);
    }

    return 0;
}
