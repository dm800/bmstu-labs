#!/usr/bin/env python
import time
import sys


def waiting_room(wait):
    print(f"STARTED TIMER FOR {wait} SECONDS")
    time.sleep(wait)
    return 0


if __name__ == "__main__":
    waiting_room(int(sys.argv[1]))
