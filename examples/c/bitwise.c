int main() {
    unsigned char a = -1;
    unsigned char b = -128;
    unsigned char c = 127;
    unsigned char d = b | a;
    unsigned char e = b & a;
    unsigned char f = b & c; 
    unsigned char g = b | c; 
    unsigned char h = -1 ^ -1; 
    unsigned char i = -1 ^ 0; 
    
    return d == 255 && e == 128 && f == 0 && g == 255 && h == 0 && i == 255;
}
