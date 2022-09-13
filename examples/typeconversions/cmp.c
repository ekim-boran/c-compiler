int int_greater_than(int i, unsigned int j) {
    if (i > j) return 1;
    else return 0;
}

int char_greater_than(char i, unsigned char j) {
    if (i > j) return 1;
    else return 0;
}

int main() {
    int r1 = int_greater_than(-1, 1);
    int r2 = char_greater_than(-1, 1);
    return r1 == 1 && r2 == 0;
}
