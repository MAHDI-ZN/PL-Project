/*===== Sample 9 =====*/
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
		output(res);
		if ((n * m) < 50) {
			for (i=2; i< 5; i=i+1){
				res = res + (2 * c[i]);
				output(res);
			}
		} else {
			for (i=2; i< 5; i=i+1){
				res = res + c[i];
			}
		}
	} else {
		res = c[3] - c[4];
		output(res);
		for (i=0; i< 3; i=i+1){
			res = res + (3 * c[i]);
			output(res);
		}
	}
	return res;
}


void main(void){
	int m;
	int n;
	m = 5;
	n = 8;
	output(func(n, m) + func(n-1, m+4));
}
