#include <math.h>
#include <stdio.h>

int main()
{
    unsigned long a = 0, b = 0, m = 1, ans = 0, t = 1;
    scanf("%ld %ld %ld", &a, &b, &m);
    int cur = 0;
    for (int i = 63; i > -1; i--) {
        t = 1;
        for (int k = 0; k < i; k++) {
            t = t * 2;
        }
        if (t <= b) {
            cur = 1;
            b = b - t;
        }
        else {
            cur = 0;
        }
        ans = ((ans % m) * (2 % m)) % m;
        if (cur == 1) ans = (ans + a % m) % m;
    }
    printf("%ld\n", ans % m);
    return 0;
}