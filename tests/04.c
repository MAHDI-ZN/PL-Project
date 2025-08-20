int i;

int rec(int a, int b){
	int res;
	if (a < 3){
		res = 1;
		print(res);
	} else {
		if (15 < b) {
			res = a;
			print(res);
		} else {
			res = rec(a - 1, b + 1) * (a - 2);
		} 
	} 

	if (a < 6){
		print(res);
	} else{
		i = i + 1;
	} 
	return res;
}

void main (void) {
	int x;
	int y;
	i = 0;
	x = 7;
	y = 13;
	print(rec(x, y));
	print(i);
}
