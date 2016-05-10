public class SqListTest {

    public static void main(String[] args) throws Exception {

        System.out.println("-----Test Constructor-----");
        SqList sqList = new SqList();
        SqList sqList2 = new SqList(50);

        System.out.println("-----Test getMaxLength-----");
        System.out.println(sqList.getMaxLength());
        System.out.println(sqList2.getMaxLength());

        System.out.println("-----Test insert & get & show-----"); // 测试 insert & get 方法
        sqList.insert(10, 1);
        sqList.insert(20, 1);

        System.out.println("1 : " + sqList.get(1));
        System.out.println("2 : " + sqList.get(2));

        sqList.insert(100, 5); // 测试插入位置大于已有元素长度很多位，是否直接插入到队尾
        sqList.show();

        // sqList.insert(30, 100); // 测试插入位置大于数组长度是否抛出异常

        System.out.println("-----Test delete-----");
        for (int i = 1; i < 10; i++) {
            sqList.insert(i, i);
        }

        sqList.show();
        sqList.delete(2);
        sqList.delete(5);
        sqList.show();

        // sqList.delete(100); // 测试插入位置大于数组长度是否抛出异常
    }
}

/**
 * 顺序表
 */
class SqList {
    public static final int MAXSIZE = 20;

    private int[] data;
    private int length;
    private int maxLength;

    // 有一个长度参数的构造方法
    public SqList(int length) {
        data = new int[length];
        length = data.length;
        maxLength = length;
    }

    // 无参构造方法
    public SqList() {
        this(MAXSIZE);
    }

    // 得到位置为i的元素的值
    public int get(int location) throws Exception {
        if (location < 1 || location > this.maxLength) {
            throw new Exception("下标越界");
        }

        return this.data[location - 1];
    }

    // 将给定元素插入指定位置
    public boolean insert(int data, int location) throws Exception {
        if (this.length == this.maxLength) {
            throw new Exception("内容已满，无法继续添加");
        }

        if (location < 1 || location > this.maxLength + 1) {
            throw new Exception("下标越界");
        }

        // 如果插入的位置在当前长度之内，则需要将已有元素向后移动
        if (location <= this.length) {
            for (int i = this.length - 1; i >= location - 1; i--) {
                this.data[i + 1] = this.data[i];
            }
            this.data[location - 1] = data;
        } else { // 如果插入的位置在当前长度之外，只需将元素插入到最后一个即可
            this.data[this.length] = data;
        }

        this.length ++;

        return true;
    }

    // 将给定位置的元素删除
    public boolean delete(int location) throws Exception {
        if (this.length == 0) {
            throw new Exception("内容为空，无法删除");
        }

        if (location < 1 || location > this.length) {
            throw new Exception("下标越界");
        }

        for (int i = location - 1; i < this.length - 1; i++) {
            this.data[i] = this.data[i + 1];
        }

        this.length--;

        return true;
    }

    // 显示自己拥有的所有元素
    public void show() {
        System.out.print("SqList(length=" + this.length + "): ");
        for (int i = 0; i < this.length; i++) {
            System.out.print(this.data[i] + " ");
        }
        System.out.println();
    }

    /* get && set */
    public int getLength() {
        return this.length;
    }

    public int getMaxLength() {
        return this.maxLength;
    }
}