int fib(int n) {
    int f;
    if (n < 2) {
	f = 1;
    } else {
        f = fib(n - 1) + fib(n - 2);
    }
    return f;
}

void main(void)
{
     print(fib(3));
}
