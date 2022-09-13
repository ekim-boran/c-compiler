typedef struct
{
  char a;
  struct
  {
    int b[4];
  };
  long c;
} Temp;

void memcpy(Temp *a, const Temp *y, int c)
{
  *a = *y;
}

int main()
{
  const Temp temp = {1, {{2, 3, 4, 5}}, 6};

  Temp temp2;
  temp2 = temp;

  int sum = temp2.a + temp2.b[2] + temp2.c;

  return sum == 11;
}
