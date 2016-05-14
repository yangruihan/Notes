#!/usr/bin/env python3
# -*- coding:utf-8 -*-


from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *


def init():
    """初始化函数"""
    glClearColor(1, 1, 1, 1)

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-50, 50, -50, 50, -1, 1)
    glMatrixMode(GL_MODELVIEW)
    glutPostRedisplay()


def line_dda(x1, y1, x2, y2):
    """使用DDA算法绘制线段"""
    dx = x2 - x1
    dy = y2 - y1
    if dx != 0:
        m = dy * 1.0 / dx
        if -1 <= m <= 1:
            if dx < 0:
                x1, x2 = x2, x1
                y = y2
            else:
                y = y1

            for x in range(x1, x2 + 1):
                glVertex2i(int(x), int(y + 0.5))
                y += m
        else:
            m = 1.0 / m
            x = x1
            if dy < 0:
                y1, y2 = y2, y1
                x = x2
            else:
                x = x1

            for y in range(y1, y2 + 1):
                glVertex2i(int(x), int(y + 0.5))
                x += m
    else:
        if dy < 0:
            y1, y2 = y2, y1
            x = x1

            for y in range(y1, y2 + 1):
                glVertex2i(int(x), int(y + 0.5))


def display():
    """显示回调函数"""
    glClear(GL_COLOR_BUFFER_BIT)

    glBegin(GL_POINTS)

    glColor3f(1, 0, 0)
    line_dda(-20, -20, 30, 25)

    glEnd()
    glFlush()


def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB)

    glutCreateWindow("dda_algorithm")
    glutInitWindowSize(100, 100)

    glutDisplayFunc(display)

    init()

    glutMainLoop()

if __name__ == '__main__':
    main()