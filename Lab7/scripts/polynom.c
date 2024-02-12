#include <math.h>
#include <stdio.h>

int main()
{
    long int ans = 0, ans2 = 0, x;
    int n;
    scanf("%d %ld", &n, &x);
    long int cur;
    for (int i = 0; i < n; i++) {
            scanf("%ld", &cur);
            ans = ans * x + cur;
            ans2 = ans2 * x + cur * (n - i);
    }
    scanf("%ld", &cur);
    ans = ans * x + cur;
    printf("%ld %ld", ans, ans2);
    return 0;
}