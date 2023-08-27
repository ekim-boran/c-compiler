int main()
{
    int result = 0;
    int c = 1;
    for (int i = 0; i < 10; i++)
    {
        if (i == 4)
            continue;
        for (int j = 0; j < 10; j++)
        {
            result += i + j;
            c = i + j;
            if (c == 3)
            {
                result += c;
            }
        }
    }

    return result;
}