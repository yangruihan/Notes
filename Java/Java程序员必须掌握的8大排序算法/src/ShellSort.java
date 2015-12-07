package Sort;

public class ShellSort {

	public static void main(String[] args) {
		int a[] = { 1, 54, 6, 3, 78, 34, 12, 45, 56, 100 };
		double d1 = a.length;
		int temp = 0;
		while (true) {
			d1 = Math.ceil(d1 / 2);
			int d = (int) d1; // ����ļ��
			// ����ÿ��
			for (int x = 0; x < d; x++) {
				for (int i = x + d; i < a.length; i += d) {
					int j;
					temp = a[i];
					// ÿ���н���ֱ�Ӳ�������
					for (j = i - d; j >= 0 && temp < a[j]; j -= d)
						a[j + d] = a[j];
					a[j + d] = temp;
				}
			}
			// �����Ϊ1ʱ�����Ѿ��������
			if (d == 1)
				break;
		}

		for (int i = 0; i < a.length; i++)
			System.out.print(a[i] + " ");
	}
}
