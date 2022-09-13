int matrix_a[500][500];
int matrix_b[500][500];
int matrix_c[500][500];

void matrix_init(int n, int nonce, int *x, int (*matrix)[500])
{
    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
        {
            matrix[i][j] = ++*x;

            if (*x % (nonce + 1))
            {
                ++*x;
            }
        }
    }
}

int matrix_mul(int n, int nonce)
{
    if (!(n <= 500))
    {
        return nonce;
    }

    int x = 0;
    matrix_init(n, nonce, &x, matrix_a);
    matrix_init(n, nonce, &x, matrix_b);

    int result = 0;
    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
        {
            matrix_c[i][j] = 0;
            for (int k = 0; k < n; ++k)
            {
                matrix_c[i][j] += matrix_a[i][k] * matrix_b[k][j];
            }
            result += matrix_c[i][j];
        }
    }

    return result;
}

int matrix_add(int n, int nonce)
{
    if (!(n <= 500))
    {
        return nonce;
    }

    int x = 0;
    matrix_init(n, nonce, &x, matrix_a);
    matrix_init(n, nonce, &x, matrix_b);

    int result = 0;
    for (int i = 0; i < n; ++i)
    {
        for (int j = 0; j < n; ++j)
        {
            matrix_c[i][j] = matrix_a[i][j] + nonce * matrix_b[i][j];
            result += matrix_c[i][j];
        }
    }

    return result;
}
void printf(char *str, unsigned long);
unsigned long clock();
int main()
{
    unsigned long start_time = clock();
    int result = matrix_mul(500, 11) + matrix_add(500, 12);
    printf("%d", (clock() - start_time));
    return result;
}
