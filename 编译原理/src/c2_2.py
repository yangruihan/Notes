#!/usr/bin/env python3
# -*-coding:utf-8 -*-

"""
第二章 一个简单的语法制导翻译器
优化版源码
- 尾递归优化
- 函数合并

expr → term rest
rest → + term { print('+') } rest
     | - term { print('-') } rest
     | ε
term → 0 { print('0') }
     | 1 { print('1') }
       ...
     | 9 { print('9') }
"""

lookahead = ''
source = ''
current_idx = 0


def expr():
    global lookahead

    term()

    while True:
        if lookahead == '+':
            match('+')
            term()
            print('+')
            continue
        elif lookahead == '-':
            match('-')
            term()
            print('-')
            continue
        else:
            break


def term():
    global lookahead

    if lookahead.isnumeric():
        t = lookahead
        match(lookahead)
        print(t)
    else:
        raise Exception('语法错误')


def match(target):
    global lookahead, source, current_idx
    if target == lookahead:
        # move lookahead
        current_idx += 1
        while current_idx < len(source) and source[current_idx].isspace():
            current_idx += 1
        lookahead = source[current_idx] if current_idx < len(source) else ''


def main():
    global lookahead, source, current_idx
    source = input('> ')
    current_idx = -1
    lookahead = ''
    match(lookahead)

    expr()


if __name__ == "__main__":
    main()
