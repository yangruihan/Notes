import java.util.Scanner;

public class GraphAdjListTest {
    public static void main(String[] args) {
        System.out.println("-----Test Constructor-----");
        GraphAdjList g = new GraphAdjList();

        System.out.println("-----Test createMGraph & show-----");
        GraphAdjList.createGraphAdjList(g);

        g.show();
    }
}

/**
 * 边表结点
 */
class EdgeNode {
    private int adjvex; // 邻接点域，存储该顶点对应的下标
    private int weight; // 权值
    private EdgeNode next; // 指针域，指向下一个邻接点

    public EdgeNode() {
    }

    /* get & set */
    public int getAdjvex() {
        return this.adjvex;
    }

    public void setAdjvex(int adjvex) {
        this.adjvex = adjvex;
    }

    public int getWeight() {
        return this.weight;
    }

    public void setWeight(int weight) {
        this.weight = weight;
    }

    public EdgeNode getNext() {
        return this.next;
    }

    public void setNext(EdgeNode next) {
        this.next = next;
    }
}

/**
 * 顶点表结点
 */
class VertexNode {
    private int data; // 数据域，存放顶点数据
    private EdgeNode firstEdge; // 边表头指针

    public VertexNode () {
    }

    /* get & set */
    public int getData() {
        return this.data;
    }

    public void setData(int data) {
        this.data = data;
    }

    public EdgeNode getFirstEdge() {
        return this.firstEdge;
    }

    public void setFirstEdge(EdgeNode firstEdge) {
        this.firstEdge = firstEdge;
    }
}

class GraphAdjList {
    private VertexNode[] adjList;
    private int numOfVexs;
    private int numOfEdges;

    public static void createGraphAdjList(GraphAdjList g) {
        Scanner scan = new Scanner(System.in);

        System.out.println("请输入顶点数和边数：");
        g.numOfVexs = scan.nextInt();
        g.numOfEdges = scan.nextInt();

        // 初始化顶点
        g.adjList = new VertexNode[g.numOfVexs];
        System.out.println("请输入顶点信息：");
        for (int i = 0; i < g.numOfVexs; i++) {
            g.adjList[i] = new VertexNode();
            g.adjList[i].setData(scan.nextInt());
            g.adjList[i].setFirstEdge(null);
        }

        System.out.println("请输入每条边的信息（起点、终点、权值）：");
        for (int i = 0; i < g.numOfEdges; i++) {
            int begin = scan.nextInt();
            int end = scan.nextInt();
            int weight = scan.nextInt();

            EdgeNode n = new EdgeNode();
            n.setWeight(weight);
            n.setAdjvex(end);
            n.setNext(g.adjList[begin].getFirstEdge());
            g.adjList[begin].setFirstEdge(n);

            n = new EdgeNode();
            n.setWeight(weight);
            n.setAdjvex(begin);
            n.setNext(g.adjList[end].getFirstEdge());
            g.adjList[end].setFirstEdge(n);
        }
    }

    public void show() {
        if (this.adjList != null) {
            System.out.println("-----MGraph-----");
            System.out.println("顶点个数：" + this.numOfVexs);
            System.out.println("边条数：" + this.numOfEdges);

            System.out.println("邻接表信息：");
            for (int i = 0; i < this.numOfVexs; i++) {
                System.out.print(this.adjList[i].getData() + "->");

                EdgeNode n = this.adjList[i].getFirstEdge();
                while (n != null) {
                    System.out.print(this.adjList[n.getAdjvex()].getData() + " ");
                    n = n.getNext();
                }

                System.out.println();
            }
        }
    }

    public GraphAdjList() {

    }

}