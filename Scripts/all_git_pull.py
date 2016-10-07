#!/usr/bin/env python3

import os
import subprocess

INIT_DIR = "/home/yrh/Workspace/"

def traversal(root_path):
    list_files = os.listdir(root_path)
    for file in list_files:
        file_path = os.path.join(root_path, file)
        if os.path.isdir(file_path):
            if os.path.exists(file_path + "/.git"):
                print("[INFO]---------------")
                print("[INFO]%s is a git repositories" % file_path)
                print("[INFO]begin git pull")
                subprocess.call("cd %s && git pull" % file_path, shell=True)
                print("[INFO]git pull finish!")
            else:
                traversal(file_path)

def main():
    traversal(INIT_DIR)

if __name__ == '__main__':
    main()