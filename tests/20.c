int func(int n, int m){
	int c[5];
	int res;
	int i;
	c[0] = 2;
	c[1] = 4;
	c[2] = 3;
	c[3] = 9;
	c[4] = 7;
	if ((n + m) < 14) {
		res = c[1] + c[0];
		print(res);
		if ((n * m) < 50) {
			i = 2;
			while (i < 5) {
				res = res + (2 * c[i]);
				print(res);
				i = i + 1;
			}
		} else {
			i = 2;
			while (i < 5) {
				res = res + c[i];
				i = i + 1;
			}
		}
	} else {
		res = c[3] - c[4];
		print(res);
		i = 0;
		while (i < 3) {
			res = res + (3 * c[i]);
			print(res);
			i = i + 1;
		}
	}
	return res;
}


void main(void){
	int m;
	int n;
	m = 5;
	n = 8;
	print(func(n, m) + func(n-1, m+4));
}
