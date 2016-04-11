import java.util.Scanner;

public class BinaryTreeTest {
    public static void main(String[] args) {
        System.out.println("-----Test Constructor-----");
        BinaryTree tree = new BinaryTree();

        System.out.println("-----Test createBiTreeByPreOrder & show-----");
        BinaryTree.createBiTreeByPreOrder(tree);

        System.out.println("\n-- Pre --");
        BinaryTree.showPreOrder(tree);

        System.out.println("\n-- In --");
        BinaryTree.showInOrder(tree);

        System.out.println("\n-- Post --");
        BinaryTree.showPostOrder(tree);
    }
}

class BinaryTree {
    private String data; // 数据域
    private BinaryTree lChild; // 左孩子
    private BinaryTree rChild; // 右孩子

    /**
     * 根据传入的前序字符串生成二叉树
     * 有数据结点输入数据
     * 虚结点输入'#'
     */
    public static void createBiTreeByPreOrder(BinaryTree tree) {
        String data;
        Scanner scan = new Scanner(System.in);
        data = scan.next();

        if (data.equals("#")) {
            tree.data = "#";
            tree.lChild = null;
            tree.rChild = null;
        }
        else {
            tree.data = data;
            tree.lChild = new BinaryTree();
            tree.rChild = new BinaryTree();
            createBiTreeByPreOrder(tree.lChild);
            createBiTreeByPreOrder(tree.rChild);
        }
    }

    /**
     * 先序遍历二叉树
     */
    public static void showPreOrder(BinaryTree root) {
        if (root == null) {
            return;
        }

        System.out.print(root.data);
        showPreOrder(root.lChild);
        showPreOrder(root.rChild);
    }

    /**
     * 中序遍历二叉树
     */
    public static void showInOrder(BinaryTree root) {
        if (root == null) {
            return;
        }

        showInOrder(root.lChild);
        System.out.print(root.data);
        showInOrder(root.rChild);
    }

    /**
     * 后序遍历二叉树
     */
    public static void showPostOrder(BinaryTree root) {
        if (root == null) {
            return;
        }

        showPostOrder(root.lChild);
        showPostOrder(root.rChild);
        System.out.print(root.data);
    }

    /* get & set */
    public String getData() {
        return this.data;
    }

    public BinaryTree getLChild() {
        return this.lChild;
    }

    public BinaryTree getRChild() {
        return this.rChild;
    }
}