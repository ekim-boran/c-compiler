

typedef struct Node
{
    char w;
    int data;
    int data2;
    struct Node *next;
} Node;

struct A
{
    char a;
    int data;
    Node node;
};

typedef struct X
{
    char a;
    int b;

} X;
typedef struct Y
{
    char a;
    int b;
    X x;
} Y;
typedef struct Z
{
    char a;
    int b;
    Y y;
} Z;
typedef struct T
{
    char a;
    int b;
    Z z;
} T;

int foo(T t)
{
    return t.z.y.x.b;
}

struct X foo2(T t)
{
    return t.z.y.x;
}

int main()
{
    X x = {12, 123};
    Y y = {1, 1, x};
    Z z = {3, 3, y};
    T t = {4, 4, z};
    Node a = {1, 10, 99};
    Node b = {1, 20, 30, &a};

    struct A a1 = {2, 11, b};

    int o = a1.node.next->data2;

    X x1 = foo2(t);

    return foo(t) + o + x1.a;
}