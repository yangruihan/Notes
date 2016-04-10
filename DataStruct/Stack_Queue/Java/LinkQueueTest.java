public class LinkQueueTest {
    public static void main(String[] args) throws Exception {
        System.out.println("-----Test Constructor-----");
        LinkQueue linkQueue = new LinkQueue();

        System.out.println("-----Test enQueue & show-----");
        for (int i = 0; i < 10; i++) {
            linkQueue.enQueue(i);
        }
        linkQueue.show();

        System.out.println("-----Test deQueue-----");
        System.out.println(linkQueue.deQueue());
        System.out.println(linkQueue.deQueue());
        System.out.println(linkQueue.deQueue());
        System.out.println(linkQueue.deQueue());
        linkQueue.show();
    }
}

/**
 * 链队列结点
 */
class QNode {
    private int data; // 数据域
    private QNode next; // 指针域

    /* get & set */
    public void setData(int data) {
        this.data = data;
    }

    public int getData() {
        return this.data;
    }

    public void setNext(QNode next) {
        this.next = next;
    }

    public QNode getNext() {
        return this.next;
    }
}

/**
 * 链队列
 */
class LinkQueue {
    private QNode front; // 头指针
    private QNode rear; // 尾指针

    public LinkQueue() {
        front = new QNode();
        front.setData(0);
        front.setNext(null);
        rear = front;
    }

    // 得到队列长度
    public int getLength() {
        int i = 0;
        QNode n = this.front.getNext();
        while (n != null) {
            n = n.getNext();
            i++;
        }

        return i;
    }

    // 入队
    public boolean enQueue(int data) {
        QNode n = new QNode();
        n.setData(data);
        n.setNext(null);

        this.rear.setNext(n);
        this.rear = n;

        return true;
    }

    // 出队
    public int deQueue() throws Exception {
        if (this.front == this.rear) { // 队列空
            throw new Exception("队列溢出（下溢）");
        }

        QNode n = this.front.getNext();
        this.front.setNext(n.getNext());

        if (n == this.rear) {
            this.rear = this.front;
        }

        return n.getData();
    }

    // 显示链队列元素内容
    public void show() {
        QNode n = this.front.getNext();
        System.out.print("LinkQueue(length=" + this.getLength() + "): ");
        while(n != null) {
            System.out.print(n.getData() + " ");
            n = n.getNext();
        }
        System.out.println();
    }
}