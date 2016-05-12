#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *


v = ((0.0, 0.0, 0.0), (25.0, 50.0, 0.0), 
     (50.0, 0.0, 0.0), (25.0, 15.0, 25.0))  # 定义一个三角形（三维）
n = 4  # 细分次数


def tetrahedron(m):
    """生成四面体"""
    glColor3f(1, 0, 0)
    divide_triangle(v[0], v[1], v[2], m)
    glColor3f(0, 1, 0)
    divide_triangle(v[3], v[2], v[1], m)
    glColor3f(0, 0, 1)
    divide_triangle(v[0], v[3], v[1], m)
    glColor3f(0, 0, 0)
    divide_triangle(v[0], v[2], v[3], m)


def divide_triangle(a, b, c, k):
    """细分三角形"""
    v1 = []
    v2 = []
    v3 = []
    if k > 0:
        for j in range(3):
            v1.append((a[j] + b[j]) / 2.0)
            v2.append((a[j] + c[j]) / 2.0)
            v3.append((b[j] + c[j]) / 2.0)

        # 细分除中间三角形的其余三角形
        divide_triangle(a, v1, v2, k - 1)
        divide_triangle(c, v2, v3, k - 1)
        divide_triangle(b, v3, v1, k - 1)
    else:
        triangle(a, b, c)


def triangle(a, b, c):
    """绘制三角形"""
    glVertex3fv(a)
    glVertex3fv(b)
    glVertex3fv(c)


def init():
    """初始化函数"""
    glClearColor(1, 1, 1, 1)  # 设置白色背景
    glColor3f(1, 0, 0)  # 设置红色笔刷

    # 初始化视图
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, 50, 0, 50, -50, 50)  # 设置视景体
    glMatrixMode(GL_MODELVIEW)


def display():
    """显示回调函数"""
    global n

    glClear(GL_COLOR_BUFFER_BIT)  # 清空帧缓存颜色

    # 开始绘制三角形
    glBegin(GL_TRIANGLES)

    tetrahedron(n)

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