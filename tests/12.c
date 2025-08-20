int foo(int x){
	if (x < 10){
		print(x);
		return 1;
	}
	else {
		print(x);
		return x + 2;
	} 
}

void main(void){
	int i;
	int j;
	i= foo(0);
	print(i);
	j=foo(12);
	print(j);
}
