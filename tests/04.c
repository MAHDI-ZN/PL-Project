/* sample 4 recursive */
int i;

int rec(int a, int b){
	int res;
	if (a < 3){
		res = 1;
		output(res);
	} else {
		if (15 < b) {
			res = a;
			output(res);
		} else {
			res = rec(a - 1, b + 1) * (a - 2);
		} 
	} 

	if (a < 6){
		output(res);
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
	output(rec(x, y));
	output(i);
}
