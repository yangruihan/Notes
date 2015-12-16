#include <stdio.h>
#include <memory.h>

int main()
{
	int i, j, k, m, n, rest, a[100][100], gain[100];
	float q[100], f[100], temp[100];
	
	memset(a, 0, sizeof(int) * 100 * 100);
	memset(gain, 0, sizeof(int) * 100);
	memset(q, 0, sizeof(float) * 100);
	memset(f, 0, sizeof(float) * 100);
	memset(temp, 0, sizeof(float) * 100);

	printf("How many item?");
	scanf("%d", &m);

	printf("How much money?");
	scanf("%d", &n);

	printf("Input one item gain table: ");
	for (j = 1; j <= n; j++)
	{
		scanf("%f", &q[j]);
		f[j] = q[j];
	}

	for (j = 0; j <= n; j++)
	{
		a[1][j] = j;
	}

	for (k = 2; k <= m; k++)
	{
		printf("Input another item gain table: ");
		for (j = 1; j <= n; j++)
		{
			temp[j] = q[j];
			scanf("%f", &q[j]);
			a[k][j] =  0;
		}
		for (j = 0; j <= n; j++)
		{
			for (i = 0; i <= j; i ++)
			{
				if (f[j - i] + q[i] > temp[j])
				{
					temp[j] = f[j - i] + q[i];
					a[k][j] = i;
				}
			}
		}
		for (j = 0; j <= n; j++)
		{
			f[j] = temp[j];
		}
	}

	rest = n;
	for (i = m; i >= 1; i --)
	{
		gain[i] = a[i][rest];
		rest -= gain[i];
	}
	for (i = 1; i <= m; i++)
	    printf("%d ", gain[i]);

	printf("\n%f", f[n]);
	return 0;
}
