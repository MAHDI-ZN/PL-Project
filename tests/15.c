void printArray(int A[], int size)
{
    int i;
    i=0;
while (i<size) {
    i=i+1;

        print(A[i]);
    }
}

void main(void)
{
    int arr[10];
    int arrsize;
    int i;
    int j;
    arrsize = 10;
    i=0;
while (i< arrsize) {
    i=i+1;

        arr[i] = 2;
        j=0;
while (j< i) {
    j=j+1;

            arr[i] = arr[i] * (0-2);
        }
    }

    printArray(arr, arrsize);
}
