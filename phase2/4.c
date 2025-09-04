string M[12];

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

void func(string dates[], int ds) {
  int sorted[ds];
  int valids;
  int idx;
  int mm;
  int dd;
  int yyyy;
  int idate;
  int swapped;
  int temp;
  int j;

  idx = 0;
  valids = 0;
  while (idx < ds) {
    if (strlen(dates[idx]) == 8) {
      idate = parseint(dates[idx]);
      dd = rem(idate, 100);
      idate = div(idate, 100);
      mm = rem(idate, 100);
      
      if (((dd >= 1) && (dd <= 31)) && ((mm >= 1) && (mm <= 12))) {
        sorted[valids] = parseint(dates[idx]);
        valids = valids + 1;
      }
    }
    idx = idx + 1;
  }

  swapped = 1;
  while (swapped == 1) {
    swapped = 0;
    j = 0;
    while (j < valids - 1) {
      if (sorted[j] > sorted[j + 1]) {
        temp = sorted[j];
        sorted[j] = sorted[j + 1];
        sorted[j + 1] = temp;
        swapped = 1;
      }
      j = j + 1;
    }
  }

  idx = 0;
  while (idx < valids) {
    idate = sorted[idx];
    dd = rem(idate, 100);
    idate = div(idate, 100);
    mm = rem(idate, 100);
    idate = div(idate, 100);
    yyyy = idate;
    
    print(M[mm-1]);
    print(dd);
    print("of");
    print(yyyy);
    print("===================");
    idx = idx + 1;
  }
}

void main(void) {
  string dates[7];
  M[0] = "Jan";
  M[1] = "Feb";
  M[2] = "Mar";
  M[3] = "Apr";
  M[4] = "May";
  M[5] = "Jun";
  M[6] = "Jul";
  M[7] = "Aug";
  M[8] = "Sep";
  M[9] = "Oct";
  M[10] = "Nev";
  M[11] = "Dec";


  dates[0] = "020250112"; dates[1] = "20190418"; dates[2] = "20230928";
  dates[3] = "20241515"; dates[4] = "20210101"; dates[5] = "20001231";
  dates[6] = "20250618";

  func(dates, 7);
}
