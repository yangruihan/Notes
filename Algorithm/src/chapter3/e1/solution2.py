# -*- coding:utf-8 -*-
def main():
    n = int(raw_input()) 
    sign = 1
    t = 1.0
    s = 1.0
    for i in range(2, n + 1):
        sign *= -1
        t *= (2 * i - 2) * (2 * i - 1)
        s += sign / t
    print s

if __name__ == '__main__':
    main()