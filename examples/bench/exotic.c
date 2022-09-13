typedef struct
{
    int a;
    int b;
} small;

typedef struct
{
    long a;
    long b;
    long c;
    long d;
    long e;
    long f;
    long g;
    long h;
} large;

typedef struct
{
    long a;
    float b;
} small_ugly;

typedef struct
{
    long a;
    float b;
    long c;
    double d;
    long e;
    long f;
    long g;
    double h;
    long i;
    long j;
    long k;
    double l;
} large_ugly;

int exotic_arguments_struct_small(small a, int nonce)
{
    return a.a + a.b + nonce;
}

long exotic_arguments_struct_large(large a, int nonce)
{
    return a.a + a.b + a.c + a.d + a.e + a.f + a.g + a.h + nonce;
}

float exotic_arguments_struct_small_ugly(small_ugly a, int nonce)
{
    return 0.0f + a.a + a.b + nonce;
}

double exotic_arguments_struct_large_ugly(large_ugly a, int nonce)
{
    return 0.0 + a.a + a.b + a.c + a.d + a.e + a.f + a.g + a.h + nonce;
}

float exotic_arguments_float(float a, int nonce)
{
    return a + (float)nonce;
}

double exotic_arguments_double(double a, int nonce)
{
    return a + (double)nonce;
}

int main()
{
    small s = {3, 4};
    large l = {5, 6, 7, 8, 9, 10, 11, 12};
    small_ugly su = {5, 6.0};
    large_ugly lu = {5, 6.0, 7, 8.0, 9, 10, 11, 12.0, 13, 14, 15, 16.0};

    int a = exotic_arguments_struct_small(s, 12);
    int b = exotic_arguments_struct_large(l, 11);
    float c = exotic_arguments_struct_small_ugly(su, 112);
    double d = exotic_arguments_struct_large_ugly(lu, 333);
    float e = exotic_arguments_float(11, 12);
    double f = exotic_arguments_double(22, 11);

    return a + b + ((int)((double)(c + e)) + e + f);
}