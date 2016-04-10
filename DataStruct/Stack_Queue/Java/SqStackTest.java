public class SqStackTest {
    public static void main(String[] args) throws Exception {

        System.out.println("-----Test Constructor & getMaxLength-----");
        SqStack sqStack = new SqStack();
        SqStack sqStack2 = new SqStack(50);

        System.out.println("S1 Max Length: " + sqStack.getMaxLength());
        System.out.println("S2 Max Length: " + sqStack2.getMaxLength());

        System.out.println("-----Test push & show-----");
        for (int i = 0; i < 10; i++) {
            sqStack.push(i);
        }
        sqStack.show();

        for (int i = 0; i < 10; i++) {
            sqStack.push(i);
        }
        sqStack.show();

        // sqStack.push(10); // 测试当栈满时，继续添加元素，抛出栈溢出异常
        // sqStack.push(11);

        System.out.println("-----Test pop-----");
        for (int i = 0; i < 20; i++) {
            System.out.println(sqStack.pop());
        }

        // sqStack.pop(); // 测试当栈空时，继续弹出元素，抛出栈溢出异常
    }
}

class SqStack {
    public static final int DEFAULT_MAXSIZE = 20;

    private int data[]; // 数据
    private int top;    // 栈顶
    private int maxLength; // 最大长度

    public SqStack(int length) {
        this.data = new int[length];
        this.top = -1;
        this.maxLength = length;
    }

    public SqStack() {
        this(DEFAULT_MAXSIZE);
    }

    // 入栈
    public boolean push(int data) throws Exception {
        if (this.top == this.maxLength - 1) { // 栈满
            throw new Exception("栈溢出（上溢）");
        }

        this.data[++this.top] = data;

        return true;
    }

    // 出栈
    public int pop() throws Exception {
        if (this.top == -1) { // 栈空
            throw new Exception("栈溢出（下溢）");
        }

        return this.data[this.top--];
    }

    // 显示栈内容
    public void show() {
        System.out.println("--Stack--");
        for (int i = this.top; i >= 0; i--) {
            System.out.println("- " + this.data[i]);
        }
        System.out.println("---------\n");
    }

    /* get & set */
    public int getMaxLength() {
        return this.maxLength;
    }
}