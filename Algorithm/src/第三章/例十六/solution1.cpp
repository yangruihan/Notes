#include <stdio.h>

int main(int argc, char* argv[])
{
    for (int i = 10000; i <= 31622; i++)
    {
        int x[10] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
        long k = i * i;
        int n, m = 0;
        while(k != 0)
        {
            if((x[n = k % 10] != 1)) break;
            else 
            {
                x[n]--;
                m++;
            }
            k /= 10;
        }
        if (m == 9)
            printf("%d: %ld\n", i, i * i);
    }
    return 0;
}
