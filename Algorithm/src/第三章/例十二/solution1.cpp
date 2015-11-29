#include <stdio.h>

int main(int argc, char* argv[])
{
    int height, a[8] = {0};
    scanf("%d", &height);
    while(height != -1)
    {
        if(height > 179) a[7]++;
        else if(height < 150) a[0]++;
        else a[height / 5 - 29]++;
        scanf("%d", &height);
    }

    for(int i = 0; i < 8; ++i)
        printf("%d\t", a[i]);

    return 0;
}
