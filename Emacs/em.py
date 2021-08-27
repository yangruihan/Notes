#!/usr/bin/env python3

import sys
import os
import uuid
import fileinput

args = sys.argv[1:]
if len(args) == 1 and args[0] == '-':
    content = []
    for line in fileinput.input():
        content.append(line)
    s = ''.join(content)
    filename = f'/tmp/emacs_pip_tmp_file_{uuid.uuid4().hex}'
    with open(f'{filename}', 'w') as f:
        f.write(s)
    os.system(f'emacsclient -t -a "" {filename}')
    os.remove(filename)
else:
    args = ' '.join(args)
    os.system(f'emacsclient -t -a "" {args}')
