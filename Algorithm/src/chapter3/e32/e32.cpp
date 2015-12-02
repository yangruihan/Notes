//============================================================================
// Name        : e32.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

int main() {
	for (int i = 0; i < 4; ++i)
		if ((i != 0) + (i == 3) + (i == 4) + (i != 4) == 3)
			cout << "小偷是：" << i << endl;
	return 0;
}
