#define OK 1
#define ERROR 0
#define TRUE 1
#define FALSE 0
#define OVERFLOW 2

typedef int Status;

#define MAX_TREE_SIZE 100

typedef int TElemtype;

/* 树的双亲表示法结点结构定义 */
typedef struct PTNode // 结点结构
{
    TElemtype data; // 结点数据
    int parent; // 双亲位置
} PTNode;

typedef struct
{
    PTNode nodes[MAX_TREE_SIZE]; // 结点数组
    int r, n;   // 根的位置和结点数
} PTree;

/* 树的孩子表示法 */
typedef struct CTNode // 孩子结点
{
    int child; // 数据域，用来存放某个结点在表头数组中的下标
    struct CTNode *next; // 指针域，用来存储指向某结点的下一个孩子结点的指针
} *ChildPtr;

typedef struct // 表头结构
{
    TElemtype data; // 数据域，用来存放某结点的数据信息
    ChildPtr firstchild; // 指针域，存储该结点的孩子链表的头指针
} CTBox;

typedef struct // 树结构
{
    CTBox nodes[MAX_TREE_SIZE]; // 结点数组
    int r, n;   // 根的位置和结点数
} CTree;

/* 树的孩子兄弟表示法 */
typedef struct CSNode
{
    TElemtype data; // 数据域
    struct CSNode *firstchild; // 指针域，用来指向该结点的第一个孩子结点
    struct CSNode *rightsib; // 指针域，用来指向该结点的右兄弟结点
} CSNode, *CSTree;
