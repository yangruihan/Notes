#!/usr/bin/env python3
# -*- coding:utf-8 -*-


class MyList(object):
    def __init__(self):
        self.data = []
        self.length = len(self.data)

    def get(self, i):
        if self.length == 0 or i < 1 or i > self.length:
            return None
        return self.data[i - 1]

    def insert(self, i, elem):
        if i < 1 or i > self.length + 1:
            return False
        if i <= self.length:
            for k in range(i - 1, self.length)[::-1]:
                self.data[k + 1] = self.data[k]

        self.data.insert(i - 1, elem)
        self.length += 1
        return True

    def delete(self, i):
        if self.length == 0:
            return None

        if i < 1 or i > self.length:
            return None

        elem = self.data[i - 1]

        if i < self.length:
            for k in range(i, self.length):
                self.data[k - 1] = self.data[k]

        self.length -= 1

        return elem
