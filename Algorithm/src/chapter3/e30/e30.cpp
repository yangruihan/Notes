//============================================================================
// Name        : e30.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

int main() {
	int a, b, c;
	cin >> a >> b >> c;
	if (a + b > c && a + c > b && b + c > a)
		if (a == b || a == c || b == c)
			if (a == b && a == c && b == c)
				cout << "等边三角形" << endl;
			else if (a * a + b * b == c * c
					|| a * a + c * c == b * b
					|| b * b + c * c == a * a)
				cout << "等腰直角三角形" << endl;
			else
				cout << "等腰三角形" << endl;
		else if (a * a + b * b == c * c
				|| a * a + c * c == b * b
				|| b * b + c * c == a * a)
			cout << "直角三角形" << endl;
		else
			cout << "普通三角形" << endl;
	else
		cout << "不能构成三角形" << endl;

	return 0;
}
