//============================================================================
// Name        : e34.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>
using namespace std;

int main() {
	int i[5], total, f;
	float n[6], p, q;
	char c[5] = { ' ', '+', '-', '*', '/' };
	cout << "Input five numbers:";
	for (int j = 1; j <= 5; ++j)
		cin >> n[j];
	cout << "Input the result:";
	cin >> n[0];
	total = 0;
	for (i[1] = 1; i[1] <= 4; i[1]++)
		if ((i[1] < 4) || (n[2] != 0))
			for (i[2] = 1; i[2] <= 4; i[2]++)
				if ((i[2] < 4) || (n[3] != 0))
					for (i[3] = 1; i[3] <= 4; i[3]++)
						if ((i[3] < 4) || (n[4] != 0))
							for (i[4] = 1; i[4] <= 4; i[4]++)
								if ((i[4] < 4) || (n[5] != 0)) {
									p = 0;
									q = n[1];
									f = 1;
									for (int k = 1; k <= 4; k++)
										switch (i[k]) {
										case 1:
											p += f * q;
											f = 1;
											q = n[k + 1];
											break;
										case 2:
											p += f * q;
											f = -1;
											q = n[k + 1];
											break;
										case 3:
											q *= n[k + 1];
											break;
										case 4:
											q /= n[k + 1];
											break;
										}
									if (p + f * q == n[0]) {
										total++;
										cout << "Total" << total << " :";
										for (int k = 1; k <= 4; k++)
											cout << n[k] << c[i[k]];
										cout << n[5] << "=" << n[0] << endl;
									}
								}
	if (total == 0)
		cout << "Non solution" << endl;
	return 0;
}
