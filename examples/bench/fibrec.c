int fibonacci_recursive(int n, int nonce)
{
    if (n < 2)
    {
        return nonce;
    }

    return fibonacci_recursive(n - 1, nonce) + fibonacci_recursive(n - 2, nonce);
}
void printf(char *str, unsigned long);
unsigned long clock();
int main()
{
    unsigned long start_time = clock();
    int result = fibonacci_recursive(36, 12);

    printf("%d", (clock() - start_time));
    return result;
}
