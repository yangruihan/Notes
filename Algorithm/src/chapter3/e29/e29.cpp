//============================================================================
// Name        : e29.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

int main() {
	int n, a[100];

	cout << "Input a number n:";
	cin >> n;

	for (int i = 0; i < n; ++i) {
		cin >> a[i];
	}

	int flag = 0;
	for (int i = 0; i < n - 1; ++i)
		for (int j = i + 1; j < n; ++j)
			if (a[i] == a[j]) {
				flag = 1;
				break;
			}

	if (flag == 1)
		cout << "repeat" << endl;
	else
		cout << "Non repeat" << endl;;

	return 0;
}
