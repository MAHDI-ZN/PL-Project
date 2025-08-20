int g(int x){
	int res;
	int j;
	int k;
	res = 0;
        j=0;
while (j< 2) {
    j=j+1;

		if (j == 1){
		} else{
			k=0;
while (k< x) {
    k=k+1;

				res = res + (k + 1);
			}
		} 
	}
	return res;
}

void f(int a, int c){
	int b[6];
	int i;
	b[0] = a;
	b[1] = c;
        i=2;
while (i< 20) {
    i=i+1;

		b[i] = b[i-1] + b[i-2];
		print(b[i]);
		if (i == 5);		
	}
	print(i + 2);
}

void main(void) {
    int x;
    int y;
    x = 1;
    y = 2;
    f(x, y);
    print(g(y + 5));
}
