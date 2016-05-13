#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from math import sin, cos

theta = 0
PI = 3.14159


def display():
    """显示函数"""
    global theta, PI

    thetar = theta * PI / 180.0
    glClear(GL_COLOR_BUFFER_BIT)

    glBegin(GL_POLYGON)
    glVertex2f(cos(thetar), sin(thetar))
    glVertex2f(-sin(thetar), cos(thetar))
    glVertex2f(-cos(thetar), -sin(thetar))
    glVertex2f(sin(thetar), -cos(thetar))
    glEnd()

    glFlush()
    glutSwapBuffers()


def idle():
    """空闲回调函数"""
    global theta

    theta += 2
    theta = theta - 360 if theta > 360 else theta
    glutPostRedisplay()



def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB)

    # 定义窗口属性
    glutInitWindowSize(500, 500)
    glutInitWindowPosition(0, 0)
    glutCreateWindow("rotating_triangle")

    glutDisplayFunc(display)
    glutIdleFunc(idle)

    glutMainLoop()


if __name__ == '__main__':
    main()