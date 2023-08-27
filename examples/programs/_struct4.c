typedef struct Temp
{
  int a;
  int b;
  struct Temp *next;
} Temp;


Temp foo(Temp a)
{
  a.a = 11;
  a.b = 12;
  return a;
}

int main()
{
  Temp t = {111, 2};
  Temp x = foo(t);
  Temp c = {1221, 2, &t};

  return foo(c).next->a;
}
