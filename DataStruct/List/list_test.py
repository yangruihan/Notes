#!/usr/bin/env python3
# -*- coding:utf-8 -*-

import unittest
from list import MyList


class TestMyList(unittest.TestCase):

    def test_insert(self):
        l = MyList()
        self.assertTrue(l.insert(1, 5))
        self.assertFalse(l.insert(0, 100))
        self.assertFalse(l.insert(100, 200))

if __name__ == '__main__':
    unittest.main()
