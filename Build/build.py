#!usr/bin/env python3

import os
import json

ITEM_TEMPLATE = "- [%s](https://github.com/yangruihan/Notes/tree/master/%s)"
global_config = None


def init_global_config():
    global global_config

    with open('config.json', 'r') as f:
        global_config = json.loads(f.read())

    print(global_config)


def main():
    global global_config

    init_global_config()

    # get all dir
    target_path = global_config['target']
    dirs = os.listdir(target_path)
    dirs = sorted(list(filter(lambda x: os.path.isdir(os.path.join(
        target_path, x)) and x not in global_config['ignore'], dirs)))

    content = "\n\n".join([ITEM_TEMPLATE % (d, d) for d in dirs])

    ret = ""
    with open('template.txt', 'r') as f:
        ret = f.read()

    ret = ret.replace("${{content}}", content)
    print(ret)

    with open(os.path.join(target_path, "README.md"), 'w') as f:
        f.write(ret)


if __name__ == "__main__":
    main()
