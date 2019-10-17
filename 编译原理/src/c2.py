#!/usr/bin/env python3
# -*-coding:utf-8 -*-

"""
第二章 一个简单的语法制导翻译器
源码

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
    term()
    rest()


def rest():
    global lookahead

    if lookahead == '+':
        match('+')
        term()
        print('+')
        rest()
    elif lookahead == '-':
        match('-')
        term()
        print('-')
        rest()
    else:
        pass


def term():
    global lookahead

    if lookahead.isnumeric():
        t = lookahead
        match(lookahead)
        print(t)
    else:
        print('Error')


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
