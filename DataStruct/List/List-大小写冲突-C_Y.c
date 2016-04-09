#define OK 1
#define ERROR 0
#define TRUE 1
#define FALSE 0

typedef int Status;

#define MAXSIZE 20

typedef int ElemType;

/* 线性表的数组表示方式 */
typedef struct
{
    ElemType data[MAXSIZE];
    int length;
} SqList;

/* 从顺序表中得到位置为i的元素 */
Status SqList_GetElem(SqList L, int i, ElemType *e)
{
    if (L.length == 0 || i < 1 || i > L.length)
        return ERROR;
    *e = L.data[i - 1];
    return OK;
}

/* 向顺序表位置i处插入元素e */
Status SqList_ListInsert(SqList *L, int i, ElemType e)
{
    int k;
    if (L->length == MAXSIZE)
        return ERROR;

    if (i < 1 || i > L->length + 1)
        return ERROR;

    if (i <= L->length)
    {
        for (k = L->length - 1; k >= i - 1; k--)
            L->data[k + 1] = L->data[k];
    }
    L->data[i - 1] = e;
    L->length++;

    return OK;
}

/* 从顺序表中删除位置为i的元素 */
Status SqList_ListDelete(SqList *L, int i, ElemType *e)
{
    int k;
    if (L->length == 0)
        return ERROR;

    if (i < 1 || i > L->length)
        return ERROR;

    *e = L->data[i - 1];
    if (i < L->length)
    {
        for (k = i; k < L->length; k ++)
            L->data[ik - 1] = L->data[k];
    }
    L->length--;

    return OK;
}

/* 线性表的单链表存储结构 */
typedef struct Node
{
    ElemType data; /* 数据域 */
    struct Node *next; /* 指针域 */
} Node;

typedef struct Node *LinkList;

/* 得到链表L的长度 */
int LinkList_ListLength(LinkList L)
{
    LinkList p;
    int num = 0;
    p = L;
    while (p->next)
    {
        num++;
        p = p->next;
    }

    return num;
}

/* 从单链表中得到位置为i的元素 */
Status LinkList_GetElem(LinkList L, int i, ElemType *e)
{
    if (i < 1 || i > LinkList_ListLength(L))
        return ERROR;

    int j = 1;
    LinkList p = L->next;

    while (p && j < i)
    {
        p = p->next;
        j++;
    }

    if (!p || j > i)
        return ERROR;

    *e = p->data;

    return OK;
}

/* 向单链表中第i个位置插入元素e */
Status LinkList_ListInsert(LinkList *L, int i, ElemType e)
{
    if (i < 1 || i > LinkList_ListLength(L) + 1)
        return ERROR;

    int j = 1;
    LinkList p = L, s;

    while (p->next && j < i)
    {
        p = p->next;
        j++;
    }

    if (!p || j > i)
        return ERROR;

    s = (LinkList)malloc(sizeof(Node));
    s->data = e;
    s->next = p->next;
    p->next = s;

    return OK;
}

/* 删除单链表中位置为i的元素 */
Status LinkList_ListDelete(LinkList *L, int i, ElemType *e)
{
    if (i < 1 || i > LinkList_ListLength(L))
        return ERROR;

    int j = 1;
    LinkList p = L;

    while(p->next && j < i)
    {
        p = p->next;
        j++;
    }

    if (!(p->next) || j > i)
        return ERROR;

    LinkList s = p->next;
    p->next = s->next;
    *e = s->data;
    free(s);

    return OK;
}

/* 头插法创建单链表 */
void CreateListHead(LinkList *L, int n)
{
    LinkList p;
    int i;
    *L = (LinkList)malloc(sizeof(Node));
    (*L)->next = NULL;

    for (i = 0; i < n; i++)
    {
        p = (LinkList)malloc(sizeof(Node));
        p->data = i;
        p->next = (*L)->next;
        (*L)->next = p;
    }
}

/* 尾插法创建单链表 */
void CreateListTail(LinkList *L, int n)
{
    LinkList p, r;
    int i;
    *L = (LinkList)malloc(sizeof(Node));
    (*L)->next = NULL;
    r = (*L);

    for (i = 0; i < n; i++)
    {
        p = (LinkList)malloc(sizeof(Node));
        p->data = i;
        p->next = NULL;
        r->next = p;
        r = p;
    }
}

/* 单链表整表删除 */
Status ClearList(LinkList *L)
{
    LinkList p, q;
    p = (*L)->next;
    while(p)
    {
        q = p->next;
        free(p);
        p = q;
    }
    (*L)->next = NULL;
    return OK;
}

int main(int argc, char const *argv[])
{
    LinkList L;
    LinkList p;
    LinkList q;

    p = (LinkList)malloc(sizeof(Node));
    p->data = 1;
    p->next=NULL;

    L->next = p;

    q = (LinkList)malloc(sizeof(Node));
    q->data = 2;
    q->next = NULL;

    p->next = q;

    printf("%d\n", LinkList_ListLength(L));

    return 0;
}