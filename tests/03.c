/* sample 3 recursive */

int count;
int result[50];


int f(int a, int result[]) {
	count = count + 1;
	if(result[a - 1]){
		return result[a - 1];
	} else {
		if (a == 1) {
			result[a - 1] = 1;
			return 1;
		} else {
            if (a == 2) {
                result[a - 1] = 1;
                return 1;
            } else {
                result[a - 1] = f(a - 2, result) + f(a - 1, result);
                return result[a - 1];
            } 
		} 
	} 
}

void main (void) {
	int i;
	for (i = 0; i < 50; i = i + 1){
		result[i] = 0;
	}
	count = 0;
	output(f(40, result));
	output(count);
}

