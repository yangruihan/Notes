#!/usr/bin/env python
#-*- coding:utf-8 -*-

import sys

s = '''#include <stdio.h>

int main(int argc, char* argv[])
{

    return 0;
}
'''

if len(sys.argv) < 2:
    print 'Please input a filename'
else:
    filename = sys.argv[1] if (str(sys.argv[1])).endswith('.cpp') else str(sys.argv[1]) + '.cpp'
    with open(filename, 'w') as f:
        f.write(s);
