#define OK 1
#define ERROR 0
#define TRUE 1
#define FALSE 0
#define OVERFLOW 2

typedef int Status;

#define MAXSIZE 20

typedef int QElemType;

typedef struct
{
    QElemType data[MAXSIZE];
    int front; // 头指针
    int rear; // 尾指针，若队列不空，指向队列尾元素的下一个位置
} SqQueue;

/* 循环队列初始化方法 */
Status SqQueue_InitQueue(SqQueue *Q)
{
    Q->front = 0;
    Q->rear = 0;

    return OK;
}

/* 循环队列求元素个数方法 */
int SqQueue_QueueLength(SqQueue Q)
{
    return (Q->rear - Q->front + MAXSIZE) % MAXSIZE;
}

/* 入队操作 */
Status SqQueue_EnQueue(SqQueue *Q, QElemType e)
{
    if ((Q->rear + 1) % MAXSIZE == Q->front) // 队列是否已经满了
        return ERROR;

    Q->data[Q->rear] = e;
    Q->rear = (Q->rear + 1) % MAXSIZE; // 循环队列

    return OK;
}

/* 出队操作 */
Status SqQueue_DeQueue(SqQueue *Q, QElemType *e)
{
    // if (QueueLength(*Q) == 0) // 队列是否为空
    if (Q->rear == Q->front) // 队列是否为空
        return ERROR;

    *e = Q->data[Q->front];
    Q->front = (Q->front + 1) % MAXSIZE;

    return OK;
}

/* 链队列 */
typedef struct QNode
{
    QElemType data;
    struct QNode *next;
} QNode, *QueuePtr;

typedef struct
{
    QueuePtr front, rear;
} LinkQueue;

/* 入队操作 */
Status LinkQueue_EnQueue(LinkQueue *Q, QElemType e)
{
    QueuePtr p = (QueuePtr)malloc(sizeof(QNode));

    if (!p) // 存储空间分配失败
        exit(OVERFLOW);

    p->data = e;
    p->next = NULL;

    Q->rear->next = p;
    Q->rear = p;

    return OK;
}

/* 出队操作 */
Status LinkQueue_DeQueue(LinkQueue *Q, QElemType *e)
{
    if (Q->rear == Q->front) // 队列为空
        return ERROR;

    QueuePtr p;
    p = Q->front->next;
    Q->front->next = p->next;
    *e = p->data;

    if (Q->rear == p)
        Q->rear = Q->front;

    free(p);

    return OK;
}