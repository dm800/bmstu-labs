#!/usr/bin/env python
import sys
import generation


if __name__ == "__main__":
    for i in range(int(sys.argv[1])):
        print(generation.generate_combination(int(sys.argv[2])))
