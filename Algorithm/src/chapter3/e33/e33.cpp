//============================================================================
// Name        : e33.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

int main() {
	for (int i = 1; i <= 4; ++i)
		for (int j = 1; j <= 4; ++j)
			if (i != j)
				for (int k = 1; k <= 4; ++k)
					if ((i == 1) + (j == 3) == 1
							&& (k == 1) + (10 - i - j - k == 4) == 1
							&& (10 - i - j - k == 2) + (i == 3) == 1)
						cout << "A: " << i << endl << "B: " << j << endl
								<< "C: " << k << endl << "D: " << 10 - i - j - k
								<< endl;
	return 0;
}
