int f(int b){
	return (b * 2);
}

void main(void) {
	int i;
	int out;
	int a[10];
	a[0] = 2;
	for(i = 2; i < 10; i = i + 2){
		int res;
		res = 0;
		out = out + i;
		if (res < 10){
			output(res);
		} else {
			break;
		} 
	}
	break;
	output(res);
	i = a + out;
	output(f(a));
}
