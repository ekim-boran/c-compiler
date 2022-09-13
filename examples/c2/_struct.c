typedef struct
{
  char a;
  struct
  {
    int b[4][5];
  };
  double c;
} Temp;

void init(int row, int col, int arr[4][5])
{
  for (int i = 0; i < row; i++)
  {
    for (int j = 0; j < col; j++)
    {
      arr[i][j] = i * j;
    }
  }
}
int memcpy(Temp *a1, Temp *a2, int size)
{
  *a1 = *a2;
}

int main()
{
  Temp temp;
  int row = 4, col = 5;
  init(row, col, temp.b);

  Temp temp2;
  temp2 = temp;

  memcpy(&temp, &temp2, 96);
  return temp2.b[2][3] == 6;
}
