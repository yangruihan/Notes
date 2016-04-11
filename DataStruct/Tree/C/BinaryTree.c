#define OK 1
#define ERROR 0
#define TRUE 1
#define FALSE 0
#define OVERFLOW 2

typedef int Status;

#define MAX_TREE_SIZE 100

typedef int TElemtype;

/* 二叉树的二叉链表结点 */
typedef struct BiTNode // 结点结构
{
    TElemtype data; // 数据域
    struct BiTNode *lchild; // 指针域，指向左孩子
    struct BiTNode *rchild; // 指针域，指向右孩子
} BiTNode, *BiTree;

/* 二叉树的前序遍历递归算法 */
void PreOrderTraverse(BiTree T)
{
    if (T == NULL)
        return;

    printf("%c", T->data); // 先输出该结点数据
    PreOrderTraverse(T->lchild); // 递归遍历该结点的左子树
    PreOrderTraverse(T->rchild); // 递归遍历该结点的右子树
}

/* 二叉树的中序遍历递归算法 */
void InOrderTraverse(BiTree T)
{
    if (T == NULL)
        return;

    InOrderTraverse(T->lchild); // 递归遍历该结点的左子树
    printf("%c", T->data); // 输出该结点的数据
    InOrderTraverse(T->rchild); // 递归遍历该结点的右子树
}

/* 二叉树的后序遍历递归算法 */
void PostOrderTraverse(BiTree T)
{
    if (T == NULL)
        return;

    PostOrderTraverse(T->lchild);
    PostOrderTraverse(T->rchild);
    printf("%c", T->data);
}

/* 二叉树的建立 */
void CreateBiTree(BiTree *T)
{
    TElemtype ch;
    scanf("%c", &ch);

    if (ch == '#')
        *T = NULL;
    else
    {
        *T = (BiTree)malloc(sizeof(BiTNode));

        if (!*T)
            exit(OVERFLOW);

        (*T)->data = ch; // 生成根结点
        CreateBiTree(&(*T)->lchild); // 递归构造左子树
        CreateBiTree(&(*T)->rchild); // 递归构造右子树ava
    }
}