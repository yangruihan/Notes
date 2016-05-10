public class LinkListTest {
    public static void main(String[] args) throws Exception {
        System.out.println("-----Test Constructor-----");
        LinkList linkList = new LinkList();

        System.out.println("-----Test getLength-----");
        System.out.println(linkList.getLength());

        System.out.println("-----Test insert & show-----");
        // 头插法
        for (int i = 0; i < 10; i++) {
            linkList.insert(i, 1);
        }
        linkList.show();

        linkList.insert(100, 100);
        linkList.show();

        // linkList.insert(200, -1);
        // linkList.show();

        System.out.println("-----Test get-----");
        System.out.println("5 : " + linkList.get(5));
        System.out.println("7 : " + linkList.get(7));

        // linkList.get(0);
        // linkList.get(100);

        System.out.println("-----Test clear-----");
        linkList.show();
        linkList.clear();
        linkList.show();
    }
}

class LinkList {
    private int data; // 数据域
    private LinkList next; // 指针域

    // 得到链表的长度
    public int getLength() {
        int length = 0;
        LinkList p = this;
        while(p.next != null) {
            p = p.next;
            length++;
        }

        return length;
    }

    // 向链表第i个位置插入元素
    public boolean insert(int data, int location) throws Exception {
        if (location < 1) {
            throw new Exception("下标越界");
        }

        int j = 1;
        LinkList p = this;
        while(p.next != null && j < location) {
            p = p.next;
            j++;
        }

        LinkList newNode = new LinkList();
        newNode.data = data;
        newNode.next = p.next;
        p.next = newNode;

        return true;
    }

    // 得到位置i的元素的值
    public int get(int location) throws Exception {
        if (location < 1 || location > this.getLength()) {
            throw new Exception("下标越界");
        }

        int j = 1;
        LinkList p = this.next;
        while(p != null && j < location) {
            p = p.next;
            j++;
        }

        return p.data;
    }

    // 显示链表所有元素
    public void show() {
        System.out.print("LinkList(length=" + this.getLength() + "): ");

        LinkList p = this.next;
        while (p != null) {
            System.out.print(p.data + " ");
            p = p.next;
        }

        System.out.println();
    }

    // 清空整张表
    public boolean clear() {
        this.next = null;
        return true;
    }

    /* get & set */
    public void setData(int data) {
        this.data = data;
    }

    public int getData() {
        return this.data;
    }

    public void setNext(LinkList next) {
        this.next = next;
    }

    public LinkList getNext() {
        return this.next;
    }
}