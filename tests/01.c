int func(int a){
	int result;
	if (a < 2){
		result = a;
	} else {
		result = func(a - 2) + (a + 1);
	}
	print(result);
	return result;
}

void main(void) {
    int a;
    a = 5;
    func(a);
}
