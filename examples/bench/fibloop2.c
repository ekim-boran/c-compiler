int fibonacci_loop(int n, int nonce)
{
    int result = 0;

    for (int step = 0; step < 10; ++step)
    {
        int x = nonce;
        int y = nonce;

        for (int i = 1; i < n; ++i)
        {
            int newy = x + y;
            x = y;
            y = newy;
        }

        result += y;
    }

    return result;
}

void printf(char *str, unsigned long);
unsigned long clock();
int main()
{
    unsigned long start_time = clock();
    int result = fibonacci_loop(2000000, 12);

    printf("%d", (clock() - start_time));
    return result;
}
