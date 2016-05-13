#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *

h = 0
old_point = [-1] * 2  # 记录上一次移动到的点的信息
start_point = [0] * 2  # 记录初始点的信息
draw = False  # 是否需要绘制


def init():
    """初始化函数"""
    glClearColor(1, 1, 1, 1)  # 设置背景

    # 初始化视图
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, 500, 0, 500, -50, 50)  # 设置视景体
    glMatrixMode(GL_MODELVIEW)

    # 启动逻辑操作
    glEnable(GL_COLOR_LOGIC_OP)
    # 选择逻辑异或操作
    glLogicOp(GL_XOR)


def display():
    """显示回调函数"""
    glClear(GL_COLOR_BUFFER_BIT)


def draw_line(x, y):
    """绘制线段"""
    global h

    y = h - y

    glColor3f(1, 0, 0)  

    glBegin(GL_LINES)

    # 擦除上一次绘制的线段
    if old_point[0] != -1 and old_point[1] != -1:
        glVertex2f(start_point[0], start_point[1])
        glVertex2f(old_point[0], old_point[1])

    # 绘制新线段
    glVertex2f(start_point[0], start_point[1])
    glVertex2f(x, y)

    glEnd()
    glFlush()

    old_point[0] = x
    old_point[1] = y


def reshape(width, height):
    """窗口大小改变回调函数"""
    global h
    h = height


def mouse(btn, state, x, y):
    """鼠标回调事件"""
    global h, draw

    if btn == GLUT_LEFT_BUTTON:
        if state == GLUT_DOWN:
            y = h - y
            start_point[0] = x
            start_point[1] = y
            draw = True
        elif state == GLUT_UP:
            old_point[0] = -1
            old_point[1] = -1
            draw = False


def mouse_move(x, y):
    """鼠标移动事件"""
    global draw
    if draw:
        draw_line(x, y)


def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB)  # 设置窗口的显示模式
    
    # 定义窗口属性
    glutInitWindowSize(500, 500)
    glutInitWindowPosition(0, 0)
    glutCreateWindow("simple_draw_rect.py")

    glutDisplayFunc(display)
    glutMouseFunc(mouse)
    glutMotionFunc(mouse_move)
    glutReshapeFunc(reshape)
    init()
    glutMainLoop()


if __name__ == '__main__':
    main()
