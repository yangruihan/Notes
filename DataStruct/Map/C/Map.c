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