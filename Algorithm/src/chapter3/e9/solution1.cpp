#include <stdio.h>

void Try(int n, int r)
{
    if(n == 1) 
        switch(r)
        {
        case 0: { printf("2(0)"); break; }
        case 1: { printf("2"); break; }
        case 2: { printf("2(2)"); break; }
        default: { printf("2("); Try(r, 0); printf(")"); break; }
        }
    else
    {
        Try(n / 2, r + 1);
        if (n % 2 == 1)
            switch(r)
            {
            case 0: { printf("+2(0)"); break; }
            case 1: { printf("+2"); break; }
            case 2: { printf("+2(2)"); break; }
            default: { printf("+2("); Try(r, 0); printf(")"); break; }
            }
    }
}

int main(int argc, char* argv[])
{
    int n;
    scanf("%d", &n);
    Try(n, 0);
    return 0;
}
