/*===== Sample 7 =====*/
int ff(int a[], int b){
	int res;
	res = a[0] + a[1];
	res = res * b;
	return res;
}

int gg(int x){
	int a;
	a = 10;
	output(x);
	if(a < x)
		return(0-1);
	if (x < a)
		return(0-2);
}

void main(void){
	int a[5];
	int i;
	int b;
	int y;
	b = 4;
	i = 0;
	y = 5;
        for (i=0; i< 5; i=i+1){
		a[i] = 1;
	}
	a[1] = 5;
	
	output(ff(a, b));
	output(gg(y));
	output(ff(a, y) * gg(b));
}
