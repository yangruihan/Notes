package Sort;

public class SelectionSort {

	public static void main(String[] args) {
		int a[] = { 1, 54, 6, 3, 78, 34, 12, 45 };
		for (int i = 0; i < a.length; i++) {
			int min = a[i];
			int t = i;
			for (int j = i + 1; j < a.length; j++) {
				if (min > a[j]) {
					min = a[j];
					t = j;
				}
			}
			a[t] = a[i];
			a[i] = min;
		}
		for (int i = 0; i < a.length; i++)
			System.out.print(a[i] + " ");
	}
}
