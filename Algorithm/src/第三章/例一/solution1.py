# -*- coding:utf-8 -*-
def main():
    n = raw_input()
    s = 1.0
    for i in range(2, int(n) + 1):
        # 求阶乘
        t = reduce(lambda x, y: x * y, range(2, 2 * i)) * 1.0
        # 求符号
        sign = 1 if (i + 1) % 2 == 0 else -1
        s += sign / t
    print s

if __name__ == '__main__':
    main()