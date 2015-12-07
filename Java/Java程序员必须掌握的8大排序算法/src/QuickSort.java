package Sort;

public class QuickSort {
    public static void main(String[] args) {
        int a[] = {49, 38, 65, 97, 76, 13, 27, 49, 78, 34, 12, 64, 5, 4, 62, 99, 98, 54, 56, 17, 18, 23, 34, 15, 35, 25, 53, 51};
        QuickSort quickSort = new QuickSort();
        quickSort.quickSort(a);
        for (int i = 0; i < a.length; i++) {
            System.out.print(a[i] + " ");
        }
    }

    public void quickSort(int[] a) {
        if (a.length > 0) {
            _quickSort(a, 0, a.length - 1);
        }
    }

    public void _quickSort(int[] list, int low, int high) {
        if (low < high) {
            int middle = getMiddle(list, low, high);
            _quickSort(list, low, middle - 1);
            _quickSort(list, middle + 1, high);
        }
    }

    public int getMiddle(int[] list, int low, int high) {
        int temp = list[low]; // 数组的第一个元素作为中轴
        while (low < high) {
            while (low < high && list[high] >= temp) {
                high--;
            }
            // 比中轴小的记录移植低端
            list[low] = list[high];
            while (low < high && list[low] <= temp) {
                low++;
            }
            // 比中轴大的记录移植高端
            list[high] = list[low];
        }
        list[low] = temp; // 中轴记录到尾
        return low;
    }
}
