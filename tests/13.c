/*==== Sample 2 =====*/
int foo(int x){
	int a;
	a = 15;
	output(x);
	if(a < x + 3){
		return(0-1);
	}
	else {
		return(0-2);
	} 
}
void main ( void )
{
	int a;
    int b;
    int c;

	int i;
	i = foo (8) ;
	output (i);

	a = 7;
    b = 2;
    c = a + b;
    if(c < 8){
    output(b - a);
    } else {
    output(c * 4);
    }

}
