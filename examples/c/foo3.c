int nonce = 12;
int g = 10;

int foo(int i, int j)
{
    return i + j + nonce;
}
int main()
{
    int i = g;

    return foo(i, i);
}