#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *


vertices = ((0.0, 0.0, 0.0), (25.0, 50.0, 0.0), 
            (50.0, 0.0, 0.0), (25.0, 15.0, 25.0))  # 定义一个三角形（三维）
colors = ((1, 0, 0), (0, 1, 0),
         (0, 0, 1), (1, 1, 0))  # 定义4种颜色
n = 4  # 细分次数


def tetra(a, b, c, d):
    """绘制四面体"""
    glColor3fv(colors[0])
    triangle(a, b, d)
    glColor3fv(colors[1])
    triangle(a, c, d)
    glColor3fv(colors[2])
    triangle(b, c, d)
    glColor3fv(colors[3])
    triangle(a, b, c)


def divide_tetra(a, b, c, d, k):
    """细分四面体"""
    mid = [[] for i in range(6)]
    if k > 0:
        for j in range(3):
            mid[0].append((a[j] + b[j]) / 2.0)
            mid[1].append((a[j] + c[j]) / 2.0)
            mid[2].append((a[j] + d[j]) / 2.0)
            mid[3].append((b[j] + c[j]) / 2.0)
            mid[4].append((b[j] + d[j]) / 2.0)
            mid[5].append((c[j] + d[j]) / 2.0)

        divide_tetra(a, mid[0], mid[1], mid[2], k - 1)
        divide_tetra(mid[0], b, mid[3], mid[4], k - 1)
        divide_tetra(mid[1], mid[3], c, mid[5], k - 1)
        divide_tetra(mid[2], mid[4], mid[5], d, k - 1)
    else:
        tetra(a, b, c, d)



def triangle(a, b, c):
    """绘制三角形"""
    glVertex3fv(a)
    glVertex3fv(b)
    glVertex3fv(c)


def init():
    """初始化函数"""
    glClearColor(1, 1, 1, 1)  # 设置白色背景

    # 激活Z-Buffer隐藏面消除算法
    glEnable(GL_DEPTH_TEST)


def display():
    """显示回调函数"""
    global n

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)  # 清空帧缓存颜色及深度缓冲区信息

    # 开始绘制三角形
    glBegin(GL_TRIANGLES)

    divide_tetra(vertices[0], vertices[1],
                 vertices[2], vertices[3], n)

    glEnd()
    glFlush()


def reshape(w, h):
    """窗口改变回调函数"""
    glViewport(0, 0, w, h)  # 设置视口
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()

    # 保证视景体的宽高比与视口的宽高比相同
    if w <= h:
        glOrtho(0, 50, 0 * h / w, 50 * h / w, -50.0, 50.0)
    else:
        glOrtho(0 * w / h, 50 * w / h, 0, 50, -50.0, 50.0)

    glMatrixMode(GL_MODELVIEW)
    glutPostRedisplay()  # 调用显示回调函数


def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB | GLUT_DEPTH)  # 设置窗口的显示模式，开启深度

    # 定义窗口属性
    glutInitWindowSize(500, 500)
    glutInitWindowPosition(0, 0)
    glutCreateWindow("sierpinski_2d_surface")

    glutReshapeFunc(reshape)  # 窗口改变回调函数
    glutDisplayFunc(display)  # 显示回调函数

    init()  # 设置 OpenGL 状态

    glutMainLoop()  # 进入事件循环


if __name__ == '__main__':
    main()