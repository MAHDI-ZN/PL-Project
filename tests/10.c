int f(void){
	print("haha");
	return 2;}
void main(void){
	int x;
	int y;
	x = 0;
	y = x * f() * f() * f();
}
