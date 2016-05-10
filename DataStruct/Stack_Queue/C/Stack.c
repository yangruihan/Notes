#define OK 1
#define ERROR 0
#define TRUE 1
#define FALSE 0

typedef int Status;

#define MAXSIZE 20

typedef int SElemType;

/* 顺序栈结构 */
typedef struct
{
    SElemType data[MAXSIZE];
    int top;
} SqStack;

/* 顺序栈 Push 操作 */
Status SqStack_Push(SqStack *S, SElemType e)
{
    if (S->top == MAXSIZE - 1) // 栈满
        return ERROR;

    S->top++;
    S->data[S->top] = e;

    return OK;
}

/* 顺序栈 Pop 操作 */
Status SqStack_Pop(SqStack *S, SElemType *e)
{
    if (S->top == -1) // 栈空
        return ERROR;

    *e = S->data[S->top];
    S->top--;

    return OK;
}

/* 两栈共享空间结构 */
typedef struct
{
    SElemType data[MAXSIZE];
    int top1;
    int top2;
} SqDoubleStack;

/* 两栈共享 Push 操作 */
Status SqDoubleStack_Push(SqDoubleStack *S, SElemType e, int stackNumber)
{
    if (S->top1 + 1 == S->top2) // 栈满
        return ERROR;

    if (stackNumber == 1) // 栈1插入元素
        S->data[++S->top1] = e;
    else if (stackNumber == 2) // 栈2插入元素
        S->data[--S->top2] = e;

    return OK;
}

/* 两栈共享 Pop 操作 */
Status SqDoubleStack_Pop(SqDoubleStack *S, SElemType e, int stackNumber)
{
    if (stackNumber == 1)
    {
        if (S->top1 == -1)
            return ERROR;

        *e = S->data[S->top1--];
    }
    else if (stackNumber == 2)
    {
        if (S->top2 == MAXSIZE)
            return ERROR;

        *e = S->data[S->top2++];
    }

    return OK;
}

/* 链栈 */
typedef struct StackNode
{
    SElemType data;
    struct StackNode *next;
} StackNode *LinkStackPtr;

typedef struct LinkStack
{
    LinkStackPtr top;
    int count;
} LinkStack;

/* StackEmpty 操作 */
bool StackEmpty(LinkStack *S)
{
    if (S->count == 0)
        return TRUE;
    else
        return FALSE;
}

/* Push 操作 */
Status Push(LinkStack *S, SElemType e)
{
    LinkStackPtr s = (LinkStackPtr)malloc(sizeof(StackNode));
    s->data = e;
    s->next = S->top;
    S->top = s;
    S->count++;

    return OK;
}

/* Pop 操作 */
Status Pop(LinkStack *S, SElemType *e)
{
    LinkStackPtr s;
    if (StackEmpty(*S))
        return ERROR;
    s = S->top;
    S->top = s->next;
    *e = s->data;
    free(s);
    S->count--;

    return OK;
}
