
void *malloc(int size);
void printf(char *c, int);
int asd();
typedef struct Node
{
    int data;
    struct Node *next;
} Node;

int exists(Node *head, int data)
{
    for (struct Node *i = head; i->next != 0; i = i->next)
    {
        if (i->data == data)
        {
            return 1;
        }
        else if (i->data > data)
        {
            return 0;
        }
    }
}

int main()
{
    printf("Hello world\n", 0);
    struct Node *head = (struct Node *)malloc(sizeof(struct Node));
    struct Node *head1 = head;
    for (int i = 0; i < 1000; i++)
    {
        head1->next = (struct Node *)malloc(sizeof(struct Node));
        head1->data = i * 2;
        head1 = head1->next;
    }
    int sum = 0;
    for (struct Node *i = head; i->next != 0; i = i->next)
    {
        sum += i->data;
    }
    printf("sum is %d\n", sum);

    int elemexists = exists(head, 1022);
    printf("1022 exists %d in list\n", elemexists);

    return sum;
}

int asd()
{
    return 1;
}