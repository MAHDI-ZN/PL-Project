/*===== Sample 1 =====*/
int foo(int x){
	if (x < 10){
		output(x);
		return 1;
	}
	else {
		output(x);
		return x + 2;
	} 
}

void main(void){
	int i;
	int j;
	i= foo(0);
	output(i);
	j=foo(12);
	output(j);
}
