void main(void) {
	int arr[10];
	int var1;
	int var2;
	var1 = 1;
	arr[0] = var1 = var2 = 8;
	print(var1);
	print(arr[0]);
	arr[5] = var2 = 12;
	print(var1);
	print(var2);
	print(arr[5]);
}
