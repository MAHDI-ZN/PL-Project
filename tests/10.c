int a;
int b[5];
int num;

int g(int d){
	int i;
	int out;
	i = 3;
	out = i * d;
	out = out + b[3];
	return out;
}

void f(){
	res = a * 4 + 3;
	output(res);
}

void main(void) {
    int num1;
    void num2;
    
    for (num1 = 0; num1 < 5; num1 = num1 + 1;){
    	b[num1] = num1 + 2;
    }
    f();
    output(g(b[1], b[2]));
    if ((b[2] + b[3]) < num1){
    	output(b[4]);
    	break;
    } else{
    	b[1] = 8;
    } 
}
