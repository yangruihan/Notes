#include <stdio.h>

int main(int argc, char* argv[])
{
    int sum, n, a;
    for (int i = 0; i < 50; i++)
    {
        n = 0;
        for (int j = 0; j < 5; j++)
        {
            scanf("%d", a);
            if (a >= 90) n++;
        }
        if (n >= 3) sum++;
    }
    printf("%d\n", sum);
    return 0;
}
