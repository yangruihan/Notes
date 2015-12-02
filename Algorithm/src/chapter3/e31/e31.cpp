//============================================================================
// Name        : e31.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

/**
 * 找3个数中的最大值
 */
int max(int x, int y, int z) {
	if (x > y && x > z)
		return x;
	else if (y > x && y > z)
		return y;
	else
		return z;
}

int main() {
	int x1, x2, x3, t = 1, flag, x0;

	cout << "Input 3 number:";
	cin >> x1 >> x2 >> x3;
	x0 = max(x1, x2, x3);
	for (int i = 2; i <= x0; ++i) {
		flag = 1;
		while (flag) {
			flag = 0;
			if (x1 % i == 0) {
				x1 /= i;
				flag = 1;
			}
			if (x2 % i == 0) {
				x2 /= i;
				flag = 1;
			}
			if (x3 % i == 0) {
				x3 /= i;
				flag = 1;
			}
			if (flag)
				t *= i;
		}
		x0 = max(x1, x2, x3);
	}

	cout << "Result is " << t << endl;

	return 0;
}
