/*==== Sample 10 ====*/
int g(int x){
	int res;
	int j;
	int k;
	res = 0;
        for (j=0; j< 2; j=j+1){
		if (j == 1){
			break;
		} else{
			for (k=0; k< x; k=k+1){
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
        for (i=2; i< 20; i=i+1){
		b[i] = b[i-1] + b[i-2];
		output(b[i]);
		if (i == 5)
			break;
		
	}
	output(i + 2);
}

void main(void) {
    int x;
    int y;
    x = 1;
    y = 2;
    f(x, y);
    output(g(y + 5));
}
