
void *malloc(int size);
int asd();
struct Node
{
    int data;
    struct Node *next;
};

int main()
{
    struct Node *head = (struct Node *)malloc(sizeof(struct Node));
    struct Node *head1 = head;
    for (int i = 0; i < 1000; i++)
    {
        head1->next = (struct Node *)malloc(sizeof(struct Node));
        head1->data = i;
        head1 = head1->next;
    }
    int sum = 0;
    for (struct Node *i = head; i->next != 0; i = i->next)
    {
        sum += i->data;
    }

    return sum;
}

int asd()
{
    return 1;
}