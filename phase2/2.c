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

int b2tob10(int b2) {
  int b10;
  int p2;
  p2 = 1;
  b10 = 0;

  while (b2 > 0) {
    if (rem(b2, 10) == 1) {
      b10 = b10 + p2;
    }
    b2 = div(b2, 10);
    p2 = p2 * 2;
  }

  return b10;
}

int isinlist(int a, string lst[], int lsts) {
  int idx;
  
  idx = 0;
  while (idx < lsts) {
    if (a == b2tob10(parseint(lst[idx]))) {
      return 1;
    }
    idx = idx + 1;
  }
  return 0;
}


int func(int b10[], int b10s, string b2[], int b2s) {
  int idx;
  int odds;
  int evens;

  odds = 1;
  evens = 1;

  idx=0;
  while(idx < b10s) {
    if(isinlist(b10[idx], b2, b2s) == 1) {
      print(b10[idx]);
      if(rem(b10[idx], 2) == 1) {
        odds = odds * b10[idx];
      } else {
        evens = evens * b10[idx];
      }
    }
    idx = idx + 1;
  }

  return odds + evens;
}

void main(void) {
  int b10[6];
  string b2[6];
  int x;

  b10[0] = 7;
  b10[1] = 11;
  b10[2] = 20;
  b10[3] = 16;
  b10[4] = 14;
  b10[5] = 1;

  b2[0] = "111";
  b2[1] = "10000";
  b2[2] = "10001001";
  b2[3] = "1011";
  b2[4] = "1100";
  b2[5] = "0";

  print(func(b10, 6, b2, 6));
}
