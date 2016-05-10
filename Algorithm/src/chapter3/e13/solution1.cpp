#include <stdio.h>

int main(int argc, char* argv[])
{
    int n, a[9] = {0};

    scanf("%d", &n);
    while(n != -1)
    {
        if(n > 0 && n <= 9) a[n - 1]++;
        scanf("%d", &n);
    }

    for(int i = 0; i < 9; ++i)
        if(a[i] == 3) printf("%d\n", i + 1);

    return 0;
}
