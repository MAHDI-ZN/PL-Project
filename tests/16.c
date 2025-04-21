/*=== Sample 5 == */
int abs(int a) {
    if (a < 0) {
        return 0-a;
    } else {
        return a;
    }
}

int isMultiplier(int a, int b) {
    int i;
    int step;
    int flag;

    if (b == 0) {
        return 0;
    } else {
        i = 1;
        flag = 0;
    }

    if (a < 0) {
        if (b < 0) {
            i = 1;
        } else {
            i = 0-1;
        }
    } else {
        if (b < 0) {
            i = 0-1;
        } else {
            i = 1;
        }
    }

    step = i;
    i = i - abs(i);
    if (abs(i) < abs(a) + 1) {
        for (i=0; abs(i) < abs(a) + 1; i = i + step) {
            if (i * b == a) {
                flag = 1;
                break;
            }
        }
    } else {
        return flag;
    }
    return flag;
}

void main(void) {
    int i;
    int j;
    int sum;
    for (i = 1; i < 11; i = i + 1) {
        sum = 0;
        for (j = 1; j < i + 1; j = j + 1) {
            if (isMultiplier(j, 2)) {
                sum = sum + 0;
            } else {
                sum = sum + j;
            }
        }
        if (isMultiplier(j, 2)) {
            output(sum);
        } else {
            j = 0;
        }
    }
}
