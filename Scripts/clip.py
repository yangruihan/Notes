#!/usr/bin/env python3


import os
import sys


FILE_PATH = os.path.join(os.path.expanduser('~'), '.emacs.d/.clip_temp_file')


def print_to_stdout(*a):
    print(*a, file=sys.stdout)


def main():
    if os.path.exists(FILE_PATH):
        with open(FILE_PATH, 'r') as f:
            print_to_stdout(f"{f.read()}")
    else:
        print_to_stdout('')


if __name__ == '__main__':
    main()
