#!/usr/bin/env python3

import os
import subprocess
import re

INIT_DIR = "/Users/yangruihan/Workspace/"
IGNORE_LIST_FILE_NAME = "agp_ignore.txt"

file_count = 0
ignore_file_list_pattern = []
rep_file_list = []

def read_ignore_file_list():
    global ignore_file_list_pattern

    ignore_file_list = []
    try:
        with open(IGNORE_LIST_FILE_NAME, 'r') as f:
            ignore_file_list = f.readlines()
    except:
        pass

    for p in ignore_file_list:
        ignore_file_list_pattern.append(re.compile(p.strip()))


def should_ignore(file_path):
    global ignore_file_list_pattern
    for pattern in ignore_file_list_pattern:
        if pattern.search(file_path):
            return True
    return False


def traversal(root_path):
    global file_count, rep_file_list

    list_files = os.listdir(root_path)
    for file in list_files:
        file_path = os.path.join(root_path, file)
        if os.path.isdir(file_path) and not should_ignore(file_path):
            if os.path.exists(file_path + "/.git"):
                print("[INFO]---------------")
                print("[INFO]%s is a git repository" % file_path)
                print("[INFO]begin git pull")
                subprocess.call("cd %s && git pull" % file_path, shell=True)
                print("[INFO]git pull finish!")
                file_count += 1
                rep_file_list.append(file_path)
                print("[SUC]No %s file finish!" % file_count)
            else:
                traversal(file_path)


def main():
    global rep_file_list
    
    read_ignore_file_list()
    traversal(INIT_DIR)
    
    print("\nThere are %s directories have been worked!" % file_count)
    print("----------Result----------")
    for index, file in enumerate(rep_file_list):
        print("  %d: %s" % (index + 1, file))

if __name__ == '__main__':
    main()
