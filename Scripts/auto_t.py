#!/usr/bin/env python3

import os


for root, dirs, files in os.walk('.'):
    for f in files:
        file = os.path.join(root, f)
        if file.endswith('.cs'):
            content = ''
            try:
                with open(file, 'rt', encoding='gb2312') as rf:
                    content = rf.read()
                
                with open(file, 'wt', encoding='utf-8') as wf:
                    wf.write(content)

                print(file)
            except:
                pass
