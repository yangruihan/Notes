package Sort;

public class MergeSort {
    public static void main(String[] args) {
        int a[] = {49, 38, 65, 97, 76, 13, 27, 49, 78, 34, 12, 64, 5, 4, 62, 99, 98, 54, 56, 17, 18, 23, 34, 15, 35, 25, 53, 51};
        MergeSort mergeSort = new MergeSort();
        mergeSort.mergeSort(a, 0, a.length - 1);
        for (int i = 0; i < a.length; i++) {
            System.out.print(a[i] + " ");
        }
    }

    public void mergeSort(int[] data, int left, int right) {
        if (left < right) {
            // 找出中间索引
            int center = (left + right) / 2;
            // 对左边数组进行递归
            mergeSort(data, left, center);
            // 对右边数组进行递归
            mergeSort(data, center + 1, right);
            // 合并
            merge(data, left, center, right);
        }
    }

    public void merge(int[] data, int left, int center, int right) {
        int[] tempArr = new int[data.length];
        int mid = center + 1;
        // third记录中间数组的索引
        int third = left;
        int temp = left;
        while (left <= center && mid <= right) {
            // 从两个数组中取出最小的放入中间数组
            if (data[left] <= data[mid]) {
                tempArr[third++] = data[left++];
            } else {
                tempArr[third++] = data[mid++];
            }
        }
        //剩余部分依次放入中间数组
        while (mid <= right) {
            tempArr[third++] = data[mid++];
        }
        while (left <= center) {
            tempArr[third++] = data[left++];
        }
        //将中间数组中的内容复制回原数组
        while (temp <= right) {
            data[temp] = tempArr[temp++];
        }
    }
}
