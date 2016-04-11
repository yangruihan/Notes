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


/* 二叉树的二叉线索存储结构 */

// Link == 0 表示指向左右孩子
// Thread == 1 表示指向前驱、后继
typedef enum {Link, Thread} PointerTag;

typedef struct BiThrNode
{
    TElemtype data; // 数据域
    struct BiThrNode *lchild; // 指针域，指向左孩子或前驱
    struct BiThrNode *rchild; // 指针域，指向右孩子或后继
    PointerTag LTag; // 左标记
    PointerTag RTag; // 右标记
} BiThrNode, *BiThrTree;

BiThrTree pre; // 全局变量，始终指向刚刚访问过的结点
/* 中序遍历进行中序线索化 */
void InThreading(BiThrTree p)
{
    if (p)
    {
        InThreading(p->lchild); // 递归遍历左子树

        if (!p->lchild) // 如果该结点没有左孩子，则设置前驱
        {
            p->lchild = pre;
            p->LTag = Thread; // 设置标记
        }

        if (!pre->rchild) // 如果它的前驱结点没有右孩子，则设置后继
        {
            pre->rchild = p;
            pre->RTag = Thread; // 设置标记
        }

        pre = p; // 始终记录刚刚访问的结点

        InThreading(p->rchild); // 递归遍历右子树
    }
}

/**
 * T 指向头结点，头结点左链 lchild 指向根结点
 * 头结点右链 rchild 指向中序遍历的最后一个结点
 * 中序遍历二叉线索链表表示的二叉树
 */
Status InOrderTraverse_Thr(BiThrTree T)
{
    BiThrTree p;
    p = T->lchild; // p指向根结点
    while (p != T) { // 空树或遍历结束时，p == T
        while (p->LTag == Link) // 当 LTag == 0 时，循环到中序序列第一个结点
            p = p->lchild;

        printf("%c", p->data); // 显示结点数据，可以更改为其他对结点操作

        while (p->RTag == Thread && p->rchild != T)
        {
            p = p->rchild;
            printf("%c", p->data);
        }

        p = p->rchild;
    }

    return OK;
}