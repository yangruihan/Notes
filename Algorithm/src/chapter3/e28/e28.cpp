//============================================================================
// Name        : e28.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

int main() {
	int a[100], flag = 1, n;
	cout << "Input data number(<100):" << endl;
	cin >> n;
	for (int i = 0; i < n; ++i)
		cin >> a[i];

	for (int i = 1; i <= n - 1 && flag == 1; ++i) {
		flag = 0;
		for (int j = n - 1; j >= i; j--)
			if (a[j] < a[j - 1]) {
				int t = a[j];
				a[j] = a[j - 1];
				a[j - 1] = t;
				flag = 1;
			}
	}

	for (int i = 0; i < n; i++)
		cout << a[i] << " ";

	return 0;
}
