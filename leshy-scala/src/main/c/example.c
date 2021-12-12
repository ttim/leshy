/**
ffact4: 411587873, ffact8: 7031418319358416161
ffact4_bytes: 411587873, ffact8_bytes: 7031418319358416161

verification sum: 164635149200000
ffact4 took 0.534805us

verification sum: -7741175128870491520
ffact8 took 6.148970us

verification sum: 164635149200000
ffact4_bytes took 0.532280us

verification sum: -7741175128870491520
ffact8_bytes took 6.192760us
*/
#include <stdio.h>
#include <sys/time.h>

long current_usec() {
    struct timeval res;
    gettimeofday(&res, NULL);
    long long usec = res.tv_sec*1000000LL + res.tv_usec;
    return usec;
}

long ffact4(int n) {
    int ans = 1;
    int i = n;
    while (i > 0) {
        ans *= i;
        i -= 2;
    }

    return ans;
}

void sum_int(int* arg1, int* arg2, int* dst) {
    *dst = *arg1 + *arg2;
}

void mult_int(int* arg1, int* arg2, int* dst) {
    *dst = *arg1 * *arg2;
}

void sum_long(long* arg1, long* arg2, long* dst) {
    *dst = *arg1 + *arg2;
}

void mult_long(long* arg1, long* arg2, long* dst) {
    *dst = *arg1 * *arg2;
}

long ffact4_bytes(int n) {
    char bytes[12];
    char* ref = &bytes;

    int minus2 = -2;

    *((int*) (ref + 8)) = 1;
    *((int*) (ref + 4)) = n;

    while (*((int*) (ref + 4)) > 0) {
        mult_int(ref + 4, ref + 8, ref + 8);
        sum_int(ref + 4, &minus2, ref + 4);
        // printf("i, ans: %d, %d\n", *((int*) (ref + 4)), *((int*) (ref + 8)));
    }

    return *((int*) (ref + 8));
}

long ffact8(int n) {
    long ans = 1;
    long i = n;
    while (i > 0) {
        ans *= i;
        i -= 2;
    }

    return ans;
}

long ffact8_bytes(int n) {
    char bytes[24];
    char* ref = &bytes;

    long minus2 = (long) -2;

    *((long*) (ref + 16)) = (long) 1;
    *((long*) (ref + 8)) = (long) n;

    while (*((long*) (ref + 8)) > 0) {
        mult_long(ref + 8, ref + 16, ref + 16);
        sum_long(ref + 8, &minus2, ref + 8);
        //printf("i, ans: %d, %d\n", *((long*) (ref + 8)), *((long*) (ref + 16)));
    }

    return *((long*) (ref + 16));
}

void test(int n, char* name, void* fn(int)) {
    int repeat = 200000;
    long sum = 0;

    // warmup
    for (int i = 0; i < repeat; i++) sum += (long) fn(n);

    // main
    long beginning = current_usec();
    for (int i = 0; i < repeat; i++) sum += (long) fn(n);
    long elapsed = current_usec() - beginning;

    printf("verification sum: %ld\n", sum);
    printf(name);
    printf(" took %fus\n\n", elapsed * 1.0 / repeat);
}

int main() {
    printf("ffact4: %d, ffact8: %ld\n", ffact4(10001), ffact8(10001));
    printf("ffact4_bytes: %d, ffact8_bytes: %ld\n", ffact4_bytes(10001), ffact8_bytes(10001));
    printf("\n");

    test(10001, "ffact4", *ffact4);
    test(10001, "ffact8", *ffact8);
    test(10001, "ffact4_bytes", *ffact4_bytes);
    test(10001, "ffact8_bytes", *ffact8_bytes);
}
