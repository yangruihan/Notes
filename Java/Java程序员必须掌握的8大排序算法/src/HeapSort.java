package Sort;

public class HeapSort {
	// 堆排序
	public void heapSort(int[] a) {
		// 循环建堆
		for (int i = 0; i < a.length - 1; i++) {
			// 建堆
			buildMaxHeap(a, a.length - 1 - i);
			// 交换栈顶和最后一个元素
			int temp = a[a.length - 1 - i];
			a[a.length - 1 - i] = a[0];
			a[0] = temp;
		}
	}

	// 建堆
	public void buildMaxHeap(int[] data, int lastIndex) {
		// 从lastIndex处节点的父节点开始（即从最后一个节点的父节点开始）
		for (int i = (lastIndex - 1) / 2; i >= 0; i--) {
			// k保存正在判断的节点
			int k = i;
			// 如果当前k节点的子节点存在
			while (k * 2 + 1 <= lastIndex) {
				// k节点左子节点的索引存在
				int biggerIndex = 2 * k + 1;
				// 如果biggerIndex小于lastIndex，即biggerIndex+1代表的k节点的右子节点存在
				if (biggerIndex < lastIndex) {
					// 如果右子节点的值较大
					if (data[biggerIndex] < data[biggerIndex + 1]) {
						biggerIndex++;
					}
				}
				// 如果k节点的值小于其他较大的子节点的值
				if (data[k] < data[biggerIndex]) {
					// 交换他们
					int temp = data[k];
					data[k] = data[biggerIndex];
					data[biggerIndex] = temp;
					// 将biggerIndex赋予k，开始while循环的下一次循环，重新保证k节点的值大于其左右子节点的值
					k = biggerIndex;
				} else {
					break;
				}
			}
		}
	}

	public static void main(String[] args) {
		int a[] = { 49, 38, 65, 97, 76, 13, 27, 49, 78, 34, 12, 64, 5, 4, 62, 99, 98, 54, 56, 17, 18, 23, 34, 15, 35,
				25, 53, 51 };

		HeapSort heapSort = new HeapSort();
		heapSort.heapSort(a);
		for (int i = 0; i < a.length; i++) {
			System.out.print(a[i] + " ");
		}

	}
}
