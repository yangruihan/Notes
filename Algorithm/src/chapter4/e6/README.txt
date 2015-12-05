用迭代法求方程组的根.

形式化代码如下:
{
    for (i = 0; i < n; i++)
        x[i] = 初始近似根;
    do {
        k++;
        for (i = 0; i < n; i++)
            y[i] = x[i];
        for (i = 0; i < n; i++)
            x[i] = gi(X);
        c = 0;
        for (i = 0; i < n; i++)
            c += fabs(y[i] - x[i]);
    } while(c > w && k < maxn);

    for (i = 0; i < n; i++)
        printf("%d 变量的近似根是 %d", i, x[i]);
}