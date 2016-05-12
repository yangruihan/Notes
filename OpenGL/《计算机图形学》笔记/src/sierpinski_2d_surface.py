#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *


vertices = ((0.0, 0.0), (25.0, 50.0), (50.0, 0.0))  # 定义一个三角形
n = 6  # 细分次数


def triangle(a, b, c):
    """绘制三角形"""
    glVertex2fv(a)
    glVertex2fv(b)
    glVertex2fv(c)


def divide_triangle(a, b, c, k):
    """细分三角形"""
    ab = []
    ac = []
    bc = []
    if k > 0:
        for j in range(2):
            ab.append((a[j] + b[j]) / 2.0)
            ac.append((a[j] + c[j]) / 2.0)
            bc.append((b[j] + c[j]) / 2.0)

        # 细分除中间三角形的其余三角形
        divide_triangle(a, ab, ac, k - 1)
        divide_triangle(c, ac, bc, k - 1)
        divide_triangle(b, bc, ab, k - 1)
    else:
        triangle(a, b, c)


def init():
    """初始化函数"""
    glClearColor(1, 1, 1, 1)  # 设置白色背景
    glColor3f(1, 0, 0)  # 设置红色笔刷

    # 初始化视图
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    gluOrtho2D(0, 50, 0, 50)  # 设置视景体
    glMatrixMode(GL_MODELVIEW)


def display():
    """显示回调函数"""
    global n

    glClear(GL_COLOR_BUFFER_BIT)  # 清空帧缓存颜色

    # 开始绘制三角形
    glBegin(GL_TRIANGLES)

    divide_triangle(vertices[0], vertices[1], vertices[2], n)

    glEnd()
    glFlush()


def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB)  # 设置窗口的显示模式

    # 定义窗口属性
    glutInitWindowSize(500, 500)
    glutInitWindowPosition(0, 0)

    glutCreateWindow("sierpinski_2d_surface")
    glutDisplayFunc(display)  # 显示回调函数

    init()  # 设置 OpenGL 状态

    glutMainLoop()  # 进入事件循环


if __name__ == '__main__':
    main()