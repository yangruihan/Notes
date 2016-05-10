import java.util.Scanner;

public class MGraphTest {
    public static void main(String[] args) {
        System.out.println("-----Test Constructor-----");
        MGraph g = new MGraph();

        System.out.println("-----Test createMGraph & show-----");
        MGraph.createMGraph(g);

        g.show();
    }
}

class MGraph {
    private int numOfVexs; // 图中的顶点个数
    private int numOfEdges; // 图中的边的条数
    private int[][] arc; // 邻接矩阵
    private String[] vexs; // 顶点数组

    public static boolean createMGraph(MGraph g) {
        Scanner scan = new Scanner(System.in);

        System.out.print("请输入图的顶点数和边数：");
        g.numOfVexs = scan.nextInt();
        g.numOfEdges = scan.nextInt();

        // 初始化顶点数组
        g.vexs = new String[g.numOfVexs];
        System.out.println("请输入每个顶点的值：");
        for (int i = 0; i < g.numOfVexs; i++) {
            g.vexs[i] = scan.next();
        }

        // 初始化邻接矩阵
        g.arc = new int[g.numOfVexs][g.numOfVexs];
        for (int i = 0; i < g.numOfVexs; i++)
            for (int j = 0; j < g.numOfVexs; j++) {
                if (i == j) {
                    g.arc[i][j] = 0; // 邻接矩阵中自己到自己的值为0用于标记
                } else {
                    g.arc[i][j] = Integer.MAX_VALUE; // 邻接矩阵中不能到达的点的值为无穷大
                }
            }
        System.out.println("请输入每条边的信息（起点、终点、权值）：");
        for (int i = 0; i < g.numOfEdges; i++) {
            int begin = scan.nextInt();
            int end = scan.nextInt();
            int weight = scan.nextInt();

            g.arc[begin][end] = weight;
            g.arc[end][begin] = weight; // 无向图，矩阵对称
        }

        return true;
    }

    // 展示函数
    public void show() {
        if (this.arc != null && this.vexs != null) {
            System.out.println("-----MGraph-----");
            System.out.println("顶点个数：" + this.numOfVexs);
            System.out.println("边条数：" + this.numOfEdges);
            System.out.print("顶点信息：");
            for (int i = 0; i < this.numOfVexs; i++) {
                System.out.print(this.vexs[i] + " ");
            }
            System.out.println();

            System.out.println("邻接矩阵信息：");
            for (int i = 0; i < this.numOfVexs; i++) {
                for (int j = 0; j < this.numOfVexs; j++) {
                    if (this.arc[i][j] == Integer.MAX_VALUE) {
                        System.out.print("∞ ");
                    } else {
                        System.out.print(this.arc[i][j] + " ");
                    }
                }
                System.out.println();
            }
        }
    }

    public MGraph() {
    }

}