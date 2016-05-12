#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *


def display():
    """显示回调函数"""
    glClear(GL_COLOR_BUFFER_BIT)
    glBegin(GL_QUADS)
    glVertex2d(-0.5, -0.5)
    glVertex2d(-0.5, 0.5)
    glVertex2d(0.5, 0.5)
    glVertex2d(0.5, -0.5)
    glEnd()
    glFlush()


def init():
    """设置OpenGL状态"""
    glClearColor(0, 0, 0, 1)  # 清除色（背景色）为黑色，且为不透明
    glColor3f(1, 1, 1)  # 矩形填充为白色

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-1, 1, -1, 1, -1, 1)  # 定义正交投影的视景体（也叫“视域体”）
    glMatrixMode(GL_MODELVIEW)


def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB)  # 设置窗口的显示模式

    # 定义窗口属性
    glutInitWindowSize(300, 300)
    glutInitWindowPosition(0, 0)

    glutCreateWindow("简单示例")
    glutDisplayFunc(display)  # 显示回调函数

    init()  # 设置 OpenGL 状态

    glutMainLoop()  # 进入事件循环

if __name__ == '__main__':
    main()
