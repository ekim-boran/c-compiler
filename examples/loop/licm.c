
int f(int y, int z){
  int n = 0;
  int a[10];
  int x = 0;

  for(int i = 0; i < 10; i++){
         x = y + z;
         a[i] = 6 * i + x * x;
    }
    return a[9];
}

int main(){
   
    int y = 10;
    int z = 10;
    return f(y, z);
   
}