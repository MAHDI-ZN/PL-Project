int size;
string num[7];
string first[7];
string last[7];
double score[7];
string mentee[7];

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

int indexOf(string stuNum) {
  int idx; 
  
  idx = 0;
  while (idx < size) {
    if (stuNum == num[idx]) {
      return idx;
    }
  }
  return (0 - 1);
}

double findMenteeMean(int count, double sum, string menteeNum) {
  int idx;
  string tmpNull;
  
  tmpNull = "null";
  idx = 0;
  while (idx < size) {
    if (num[idx] == menteeNum) {
      if (mentee[idx] == tmpNull) {
        return (sum + score[idx])/(count + 1);
      } else {
        return findMenteeMean(count + 1, sum + score[idx], mentee[idx]);
      }
    }
    idx = idx + 1;
  }

  return 0.0;
}

string findBest(int year) {
  double bestScore;
  double tempScore;
  string bestStu;
  int idx;

  bestScore = 0.0;
  bestStu = "";
  idx = 0;

  while (idx < size) {
    if (year == div(parseint(num[idx]), 1000000)) {
      tempScore = findMenteeMean(0, 0.0, mentee[idx]);
      if (tempScore > bestScore) {
        bestScore = tempScore;
        bestStu = num[idx];
      }
    }
    
    idx = idx + 1;
  }
  
  return bestStu;
}

void main(void) {
  int n;
  int idx;
  string best;
  n = 400;

  size = 7;

  num[0] = "99111111";
  first[0] = "person0";
  last[0] = "test0";
  score[0] = 18.98;
  mentee[0] = "403111111";

  num[0] = "99111111";
  first[0] = "person0";
  last[0] = "test0";
  score[0] = 18.98;
  mentee[0] = "403111111";

  num[1] = "400111111";
  first[1] = "person1";
  last[1] = "test1";
  score[1] = 18.12;
  mentee[1] = "401111111";

  num[2] = "400222222";
  first[2] = "person2";
  last[2] = "test2";
  score[2] = 19.00;
  mentee[2] = "402222222";

  num[3] = "401111111";
  first[3] = "person3";
  last[3] = "test3";
  score[3] = 16.55;
  mentee[3] = "402111111";

  num[4] = "402111111";
  first[4] = "person4";
  last[4] = "test4";
  score[4] = 19.72;
  mentee[4] = "null";

  num[5] = "402222222";
  first[5] = "person5";
  last[5] = "test5";
  score[5] = 17.29;
  mentee[5] = "null";

  num[6] = "403111111";
  first[6] = "person6";
  last[6] = "test6";
  score[6] = 14.84;
  mentee[6] = "null";
  

  best = findBest(400);
  
  idx = 0;
  while (idx < size) {
    if (num[idx] == best) {
      print(first[idx]);
      print(last[idx]);
    }
    idx = idx + 1;
  }
}
