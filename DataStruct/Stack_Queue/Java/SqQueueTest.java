public class SqQueueTest {
    public static void main(String[] args) throws Exception {
        System.out.println("-----Test Constructor-----");
        SqQueue sqQueue = new SqQueue();
        SqQueue sqQueue2 = new SqQueue(50);

        System.out.println("-----Test enQueue & show-----");
        for (int i = 0; i < 10; i++) {
            sqQueue.enQueue(i);
        }
        sqQueue.show();

        System.out.println("-----Test deQueue-----");
        System.out.println(sqQueue.deQueue());
        System.out.println(sqQueue.deQueue());
        System.out.println(sqQueue.deQueue());
        System.out.println(sqQueue.deQueue());
        System.out.println(sqQueue.deQueue());
        for (int i = 0; i < 14; i++) {
            sqQueue.enQueue(i);
        }
        sqQueue.show();
    }
}

/**
 * 循环队列
 */
class SqQueue {
    public static final int DEFAULT_MAXSIZE = 20;

    private int[] data; // 数据
    private int front, rear; // 头指针、尾指针
    private int maxLength; // 最大长度

    public SqQueue(int length) {
        this.data = new int[length];
        this.front = 0;
        this.rear = 0;
        this.maxLength = length;
    }

    public SqQueue() {
        this(DEFAULT_MAXSIZE);
    }

    // 获得队列元素个数
    public int getLength() {
        return (this.rear - this.front + this.maxLength) % this.maxLength;
    }

    // 入队
    public boolean enQueue(int data) throws Exception {
        if ((this.rear + 1) % this.maxLength == this.front) { // 队列满
            throw new Exception("队列溢出（上溢）");
        }

        this.data[this.rear] = data;
        this.rear = (this.rear + 1) % this.maxLength;

        return true;
    }

    // 出队
    public int deQueue() throws Exception {
        if (this.front == this.rear) { // 队列空
            throw new Exception("队列溢出（下溢）");
        }

        int i = this.data[this.front];
        this.front = (this.front + 1) % this.maxLength;

        return i;
    }

    // 显示队列内容
    public void show() {
        System.out.print("SqQueue(length=" + this.getLength() + "): ");
        int front = this.front;

        while (front != this.rear) {
            System.out.print("(" + front + ", " +this.data[front] + ") ");
            front = (front + 1) % maxLength;
        }
        System.out.println();
    }
}