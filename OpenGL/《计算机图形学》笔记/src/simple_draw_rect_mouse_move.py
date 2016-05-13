#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *

h = 0
size = 10


def init():
    glClearColor(1, 1, 1, 1)  # 设置白色背景

    # 初始化视图
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, 500, 0, 500, -50, 50)  # 设置视景体
    glMatrixMode(GL_MODELVIEW)


def display():
    glClear(GL_COLOR_BUFFER_BIT)


def draw_rect(x, y):
    """绘制矩形"""
    global h, size

    y = h - y

    glColor3f(random(), random(), random())  # 随机颜色

    glBegin(GL_POLYGON)

    glVertex2f(x - size, y - size)
    glVertex2f(x + size, y - size)
    glVertex2f(x + size, y + size)
    glVertex2f(x - size, y + size)

    glEnd()
    glFlush()


def reshape(width, height):
    """窗口大小改变回调函数"""
    global h
    h = height


def mouse_move(x, y):
    """鼠标移动事件"""
    draw_rect(x, y)


def main():
    glutInit()
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB)  # 设置窗口的显示模式
    
    # 定义窗口属性
    glutInitWindowSize(500, 500)
    glutInitWindowPosition(0, 0)
    glutCreateWindow("simple_draw_rect.py")

    glutDisplayFunc(display)
    glutMotionFunc(mouse_move)
    glutReshapeFunc(reshape)
    init()
    glutMainLoop()


if __name__ == '__main__':
    main()
