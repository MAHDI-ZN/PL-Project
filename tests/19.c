int sum(int b[], int arrsize){
	int s;
	int i;
	s = 0;
	i = 0;
	while (i < arrsize) {
		s = s + b[i];
		i = i + 1;
	}
	return s;
}

int fib(int n){
	int fib1;
	int fib2;
	int j;
	int res;
	fib1 = 1;
	fib2 = 2;
	j = 0;
	while (j < (n + 1)) {
		res = fib1 + fib2;
		print(res);
		fib1 = fib2;
		fib2 = res;
		j = j + 1;
	}
	return res;
}

void main(void){
	int a[9];
	int j;
	j = 0;
	while (j < 9) {
		a[j] = j + 1;
		j = j + 1;
	}
	print(sum(a,9));
	print(fib(3));
}
