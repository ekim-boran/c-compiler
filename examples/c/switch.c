int k = 11;
int main()
{
    int a = 1 + k;
    int b = 11;
    switch (a)
    {
    case 0:
    {
        b += 1;
        break;
    }
    case 1:
    {
        b += 2;
        break;
    }
    default:
    {
        b += 3;
        break;
    }
    }

    return b;
}
