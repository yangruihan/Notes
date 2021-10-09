#!/usr/bin/env python3

import sys
import os
import uuid
import fileinput

"""
Make emacs support for pipline

Example:
    
    man printf | em.py -

    em.py test
"""

args = sys.argv[1:]

# check use pipline?
if len(args) == 1 and args[0] == '-':
    content = []
    
    # read content from stdin
    for line in fileinput.input():
        content.append(line)
    s = ''.join(content)
    
    # create temple file
    filename = f'/tmp/emacs_pip_tmp_file_{uuid.uuid4().hex}'
    with open(f'{filename}', 'w') as f:
        f.write(s)

    # open temple file with emacsclient
    os.system(f'emacsclient -t -a "" {filename}')
    
    # remove temple file
    os.remove(filename)
else: # otherwise simple open file with args
    args = ' '.join(args)
    os.system(f'emacsclient -t -a "" {args}')
