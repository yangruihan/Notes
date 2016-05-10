#!/usr/bin/env python
# -*- coding:utf-8 -*-

import sys


def main():
    if len(sys.argv) < 2:
        print 'Please input type and filename.\nYou can use "-h" to see the help document.'
        return -1;

    if sys.argv[1] == '-h':
        print '''gen_code_template <file_type> <filename>
<file_type>:
    -cpp: create a cpp template file
    -j: create a Java template file
    -p: create a python template file
<filename>:
    The filename you would want to create.
        '''
        return 0;

    if len(sys.argv) < 3:
        print 'Please input a filename.\nYou can use "-h" to see the help document.'
        return -1;

    cpp_temp = '''#include <stdio.h>

int main(int argc, char* argv[])
{
    printf("Hello World!");
    return 0;
}
    '''

    java_temp = '''public class '''+ (str(sys.argv[2])).capitalize() + ''' {
    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}
    '''

    python_temp = '''#!/usr/bin/env python
# -*- coding:utf-8 -*-


def main():
    print('Hello World')

if __name__ == '__main__':
    main()
    '''

    filename = ""
    file_type = ""

    # 创建C++代码模板
    if sys.argv[1] == '-cpp':
        filename = sys.argv[1] if (str(sys.argv[2])).endswith('.cpp') else str(sys.argv[2]) + '.cpp'
        file_type = cpp_temp

    # 创建Java代码模板
    elif sys.argv[1] == '-j':
        filename = sys.argv[1] if (str(sys.argv[2])).endswith('.java') else str(sys.argv[2]).capitalize() + '.java'
        file_type = java_temp

    # 创建Python代码模板
    elif sys.argv[1] == '-p':
        filename = sys.argv[1] if (str(sys.argv[2])).endswith('.py') else str(sys.argv[2]) + '.py'
        file_type = python_temp

    else:
        print '''Input Type Error!\nYou can use "-h" to see the help document.'''
        return -1;

    with open(filename, 'w') as f:
        f.write(file_type);


if __name__ == '__main__':
    main()
