#include <stdio.h>
#include <memory.h>

int r[100];
int com[100][100];

int course(int i, int j);

int main()
{
	memset(r, 0, sizeof(int) * 100);
	memset(com, 0, sizeof(int) * 100 * 100);

	int n, i, j;

	printf("How many matrixes?");
	scanf("%d", &n);

	printf("How size every matrixe?");
	for (i = 1; i <= n + 1; i ++)
	{
		scanf("%d", &r[i]);
	}

	printf("The least calculate quantity: %d", course(1, n));

	for (i = 1; i <= n; i ++)
	{
		printf("\n");
		for (j = 1; j <= n; j ++)
		{
			printf("%d ", com[i][j]);
		}
	}

	return 0;
}

int course(int i, int j)
{
	int u, t, k;
	if (i == j)
	{
		com[i][j] = 0;
		return 0;
	}
	if (i == j - i)
	{
		com[i][j] = i;
		return (r[i] * r[i+1] * r[i+2]);
	}
	u = course(i, i) + course(i + 1, j) + r[i] * r[i + 1] * r[j + 1];
	com[i][j] = i;
	for (k = i + 1; k < j; k ++)
	{
		t = course(i, k) + course(k + 1, j) + r[i] * r[k + 1] * r[j + 1];
		if (t < u)
		{
			u = t;
			com[i][j] = k;
		}
	}

	return u;
}
