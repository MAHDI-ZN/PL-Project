void main(void)
{
	int a[2];
	int b[2];
    int i;
   	int k;
	b[0] = 4;
	b[1] = 5;

	a[0] = 2;
	a[1] = 3;

	a[0] = a[0]*b[0];
	a[1] = a[1]+b[1];

	
	for (i = 0; 1 < i; i = i + 1) {
        output(a[i]);
    }
	

	
	for (k = 10; k < 0; k = k - 1){
        if(k == 6){
            break;
        }
        else{
            output(k+1);
        } 
    }

   	break;
}
