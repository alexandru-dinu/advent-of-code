#include <stdio.h>
#include <math.h>

#ifdef LONG
    const int PROG[] = {2,4,1,3,7,5,1,5,0,3,4,2,5,5,3,0};
#else
    const int PROG[] = {0,3,5,4,3,0};
#endif

#define SZ(out) (sizeof(out)/sizeof(out[0]))

int main() {
#ifdef LONG
    // int out[16] = {0};
    long long k = 1;//00000000000000 / 2;
#else
    // int out[6] = {0};
    long long k = 1;
#endif

    int bst = 0;

    while (1) {
        int A = k, B = 0, C = 0, o = 0;
        int i = 0;


        while (A != 0) {
#ifdef LONG
            B = (A % 8) ^ 3;
            C = A / (1 << B);
            A /= 8;
            B = (B ^ 5) ^ C;
            o = B % 8;
#else
            A /= 8;
            o = A % 8;
#endif
            if (o != PROG[i]) {
                break;
            }
            i++;
        }

        if (i >= bst) {
            bst = i;
            printf("%d %lld %llo\n", bst, k, k);
        }

        if (i == SZ(PROG)) {
            printf(">>> %lld\n", k);
            return 0;
        }
        k++;
    }
    return 1;
}
