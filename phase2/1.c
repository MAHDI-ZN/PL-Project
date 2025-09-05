int div(int a, int b) {
  int res;
  res = 0;
    
  while (a >= b) {
    res = res + 1;
    a = a - b;
  }

  return res;
}

int rem(int a, int b) {
  int res;
  res = a - div(a, b) * b;
  return res;
}

int func(int a) {
  int res;
  res = 0;

  if (a < 0) {
    print("Please enter non-negative number");
    return (0 - 1);
  }

  while (a > 0) {
    res = res * 10 + rem(a, 10);
    a = div(a, 10);
  }
  
  return res;
}

void main(void) {
    int input;
    input = 123450;
    print(func(input));
}
