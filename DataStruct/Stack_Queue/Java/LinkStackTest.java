public class LinkStackTest {
    public static void main(String[] args) throws Exception {
        System.out.println("-----Test Constructor & empty-----");
        LinkStack linkStack = new LinkStack();
        System.out.println(linkStack.empty());

        System.out.println("-----Test push & show-----");
        for (int i = 0; i < 10; i++) {
            linkStack.push(i);
        }
        linkStack.show();
        System.out.println("Empty? " + linkStack.empty());

        System.out.println("-----Test pop-----");
        for (int i = 0; i < 10; i++) {
            System.out.println(i + " : " + linkStack.pop());
        }
        System.out.println("Empty? " + linkStack.empty());
    }
}

/**
 * 链栈结点
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

class LinkStack {
    private QNode top; // 栈顶指针
    private int count; // 记录当前栈内元素数量

    public LinkStack() {
        this.top = new QNode();
        this.top.setData(0);
        this.top.setNext(null);
        this.count = 0;
    }

    // 判断链表是否为空
    public boolean empty() {
        if (this.count == 0) {
            return true;
        } else {
            return false;
        }
    }

    // 入栈
    public void push(int data) {
        QNode n = new QNode();
        n.setData(data);
        n.setNext(this.top.getNext());
        this.top.setNext(n);
        this.count++;
    }

    // 出栈
    public int pop() throws Exception {
        if (this.empty()) { // 栈空
            throw new Exception("栈溢出（下溢）");
        }

        QNode n = this.top.getNext();
        this.top.setNext(n.getNext());
        this.count--;

        return n.getData();
    }

    // 显示栈内容
    public void show() {
        System.out.println("--Stack--");
        QNode n = this.top.getNext();
        while (n != null) {
            System.out.println("- " + n.getData());
            n = n.getNext();
        }
        System.out.println("---------\n");
    }

    /* get & set */
    public void setTop(QNode top) {
        this.top = top;
    }

    public QNode getTop(QNode top) {
        return this.top;
    }

    public int getCount() {
        return this.count;
    }
}