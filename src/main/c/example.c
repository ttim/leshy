#include <stdio.h>
#include <sys/time.h>

long current_usec() {
    struct timeval res;
    gettimeofday(&res, NULL);
    long long usec = res.tv_sec*1000000LL + res.tv_usec;
    return usec;
}

int ffact4(int n) {
    int ans = 1;
    int i = n;
    while (i > 0) {
        ans *= i;
        i -= 2;
    }

    return ans;
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

void test(int n, char* name, void* fn(int)) {
    int repeat = 10000;
    int sum = 0;

    // warmup
    for (int i = 0; i < repeat; i++) sum += (int) fn(n);

    // main
    long beginning = current_usec();
    for (int i = 0; i < repeat; i++) sum += (int) fn(n);
    long elapsed = current_usec() - beginning;

    printf("verification sum: %d\n", sum);
    printf(name);
    printf(" took %fus\n", elapsed * 1.0 / repeat);
}

int main() {
    printf("ffact4: %d, ffact8: %ld\n", ffact4(10001), ffact8(10001));

    test(10001, "ffact4", *ffact4);
    test(10001, "ffact8", *ffact8);
}
