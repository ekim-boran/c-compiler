int two_dimension_array_arr[10000];

int two_dimension_array(int n, int nonce)
{
    if (!(n <= 10000))
    {
        return nonce;
    }

    for (int i = 0; i < n; ++i)
    {
        two_dimension_array_arr[i] = i + nonce;
    }

    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
        {
            two_dimension_array_arr[i] += two_dimension_array_arr[j];
        }
    }

    int result = 0;
    for (int i = 0; i < n; ++i)
    {
        result += two_dimension_array_arr[i];
    }

    return result;
}
void printf(char *str, unsigned long);
unsigned long clock();
int main()
{
    unsigned long start_time = clock();
    int result = two_dimension_array(10000, 33);
    printf("%d", (clock() - start_time));
    return result;
}
