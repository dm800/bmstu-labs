#!/usr/bin/env python

def decor(func):
    sl = {}

    def main_func(*args):
        if args in sl:
            return sl[args]
        else:
            temp = func(*args)
            sl[args] = temp
            return temp

    return main_func


@decor
def some_hard_work(n):
    if n == 0:
        return 0
    else:
        return n + some_hard_work(n - 1)


for i in range(10000):
    print(some_hard_work(i))
