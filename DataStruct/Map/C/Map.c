#define OK 1
#define ERROR 0
#define TRUE 1
#define FALSE 0
#define OVERFLOW 2

typedef int Status;

typedef char VertexType;

typedef int EdgeType;

#define MAXVEX 100
#define INFINITY 65536

/* 图的邻接矩阵表示法 */
typedef struct
{
    VertexType vexs[MAXVEX]; // 顶点表
    EdgeType arc[MAXVEX][MAXVEX]; // 邻接矩阵，可看作边表
    int numVertexes; // 顶点数
    int numEdges; // 边数
} MGraph;

/* 建立无向图的邻接矩阵表示 */
void CreateMGraph(MGraph *G)
{
    int i, j, k, w;
    printf("输入顶点数和边数\n");

    scanf("%d, %d", &G->numVertexes, &G->numEdges);

    for (i = 0; i < G->numVertexes; i++)
        scanf(&G->vexs[i]);

    for (i = 0; i < G->numVertexes; i++)
        for (j = 0; j < G->numVertexes; j++)
            G->arc[i][j] = INFINITY; // 邻接矩阵初始化

    for (k = 0; k < G->numEdges; k++)
    {
        printf("输入边 (vi, vj) 上的下标i，下标j和权w：\n");
        scanf("%d, %d, %d", &i, &j, &w);
        G->arc[i][j] = w;
        G->arc[j][i] = G->arc[i][j]; // 因为是无向图，矩阵对称
    }
}

/* 无向图的链表表示法 */
typedef struct EdgeNode // 边表结点
{
    int adjvex; // 邻接点域，存储该顶点对应的下标
    EdgeType weight; // 存储权值
    struct EdgeNode *next; // 指针域，指向下一个邻接点
} EdgeNode;

typedef struct VertexNode // 顶点表结点
{
    VertexType data; // 顶点域，存储顶点信息
    EdgeNode *firstedge; // 边表头指针
} VertexNode, AdjList[MAXVEX];

typedef struct
{
    AdjList adjList;
    int numVertexes; // 顶点数
    int numEdges; // 边数
} GraphAdjList;

/* 无向图邻接表创建 */
void CreateALGraph(GraphAdjList *G)
{
    int i, j, k;
    EdgeNode *e;
    printf("输入顶点数和边数：\n");
    scanf("%d, %d", &G->numVertexes, &G->numEdges);

    for (i = 0; i < G->numVertexes; i++)
    {
        scanf(&G->adjList[i].data);
        G->adjList[i].firstedge = NULL;
    }

    for (k = 0; k < G->numEdges; k++)
    {
        printf("输入边 (vi, vj) 上的下标i，下标j和权w：\n");
        scanf("%d, %d", &i, &j);

        e=(EdgeNode*)malloc(sizeof(EdgeNode));

        e->adjvex = j;
        e->next = G->adjList[i].firstedge;
        G->adjList[i].firstedge = e;

        e=(EdgeNode*)malloc(sizeof(EdgeNode));

        e->adjvex = i;
        e->next = G->adjList[j].firstedge;
        G->adjList[j].firstedge = e;
    }
}

#define MAX 50;

/* 深度优先搜索 */
typedef int Boolean;
Boolean visited[MAX];

void DFS(MGraph G, int i)
{
    int j;
    visited[i] = TRUE;
    printf("%c ", G.vexs[i]);
    for (j = 0; j < G.numVertexes; j++)
        if (G.arc[i][j] == 1 && !visited[j])
            DFS(G, j);
}

void DFSTraverse(MGraph G)
{
    int i;
    for (i = 0; i < G.numVertexes; i++)
        visited[i] = FALSE;
    for (i = 0; i < G.numVertexes; i++)
        if (!visited[i]) // 对未访问过的顶点调用 DFS，若是连通图，只会执行一次ava
            DFS(G, i);
}

/* Prim 算法生成最小生成树 */
void MiniSpanTree_Prim(MGraph G)
{
    int min, i, j, k;
    int adjvex[MAXVEX]; // 保存相关顶点下标
    int lowcost[MAXVEX]; // 保存相关顶点间边的权值
    lowcost[0] = 0; // 初始化第一个权值为0，即v0已经加入生成树
    adjvex[0] = 0; // 初始化第一个顶点下标为0

    for (i = 1; i < G.numVertexes; i++) // 循环除下标为0外的全部顶点
    {
        lowcost[i] = G.arc[0][i]; // 将v0顶点与之有边的权值存入数组
        adjvex[i] = 0; // 初始化路径为0
    }

    for (i = 1; i < G.numVertexes; i++)
    {
        min = INFINITY;

        j = 1; k = 0;
        while(j < G.numVertexes)
        {
            if (lowcost[j] != 0 && lowcost[j] < min)
            {
                min = lowcost[j];
                k = j;
            }
            j++;
        }

        printf("(%d, %d) ", adjvex[k], k);
        lowcost[k] = 0;

        for (j = 1; j < G.numVertexes; j++)
        {
            if (lowcost[j] != 0 && G.arc[k][j] < lowcost[j])
            {
                lowcost[j] = G.arc[k][j];
                adjvex[j] = k;
            }
        }
    }
}

/* Kruskal 算法 */
/* 对边集数组的定义 */
typedef struct
{
    int begin;
    int end;
    int weight;
} Edge;

// 查找连线顶点的尾部下标
int Find(int *parent, int f)
{
    while (parent[f] > 0)
        f = parent[f];
    return f;
}

void MiniSpanTree_Kruskal(MGraph G)
{
    int i, n, m;
    Edge edges[MAXVEX]; // 定义边集数组
    int parent[MAXVEX]; // 定义一数组，用来判断是否与边形成环路

    // 省略将邻接矩阵转换成边集数组，并按权有小到大排序的代码

    for (i = 0; i < G.numVertexes; i++)
        parent[i] = 0; // 初始化数组为0

    for (i = 0; i < G.numEdges; i++) // 循环每一条边
    {
        n = Find(parent, edges[i].begin);
        m = Find(parent, edges[i].end);
        if (n != m)
        {
            parent[n] = m; // 将此边的结尾顶点放入下标为起点的 parent 中
            printf("(%d, %d) %d ", edges[i].begin, edges[i].end, edges[i].weight);
        }
    }
}

/* 迪杰斯特拉（Dijkstra）算法 */
typedef int Pathmatirx[MAXVEX]; // 用于存储最短路径下标
typedef int ShortPathTable[MAXVEX]; // 用于存储到各点最短路径的权值和

void ShortestPath_Dijkstra(MGraph G, int v0, Pathmatirx *P, ShortPathTable *D)
{
    int final[MAXVEX]; // final[w] = 1 表示求得顶点v0到vw的最短路径
    int k;

    for (int v = 0; v < G.numVertexes; v++) // 初始化
    {
        (*P)[v] = 0;
        (*D)[v] = G.matirx[v0][v];
        final[v] = 0;
    }

    (*D)[v0] = 0;
    final[v0] = 1;

    for (int v = 1; v < G.numVertexes; v++)
    {
        min = INFINITY;

        for (int w = 0; w < G.numVertexes; w++) // 找到距离当前已进入最短路径队列中的顶点最近的下一顶点，并赋值给K
        {
            if (!final[w] && (*D)[w] < min)
            {
                k = w;
                min = (*D)[w];
            }
        }

        final[k] = 1;

        for (int w = 0; w < G.numVertexes; w++) // 将K加入最短路径队列，并利用顶点K更新与其他顶点的距离
        {
            if (!final[w] && (min + G.matirx[k][w] < (*D)[w]))
            {
                (*D)[w] = min + G.matirx[k][w];
                (*P)[w] = k;
            }
        }
    }
}

/* 弗洛伊德（Floyd）算法 */
typedef int Pathmatirx2[MAXVEX][MAXVEX];
typedef int ShortPathTable2[MAXVEX][MAXVEX];
void ShortestPath_Floyd(MGraph G, Pathmatirx2 *P, ShortPathTable2 *D)
{
    for (int v = 0; v < G.numVertexes; v++)
        for (int w = 0; w < G.numVertexes; w++)
        {
            (*D)[v][w] = G.matirx[v][w];
            (*P)[v][w] = w;
        }

    for (int k = 0; k < G.numVertexes; k++)
        for (int v = 0; v < G.numVertexes; v++)
            for (int w = 0; w < G.numVertexes; w++)
            {
                if ((*D)[v][w] > (*D)[v][k] + (*D)[k][w])
                {
                    (*D)[v][w] = (*D)[v][k] + (*D)[k][w];
                    (*P)[v][w] = (*P)[v][k];
                }
            }
}