#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *


def display():
    glClear(GL_COLOR_BUFFER_BIT)
    glBegin(GL_QUADS)
    glVertex2d(-0.5, -0.5)
    glVertex2d(-0.5, 0.5)
    glVertex2d(0.5, 0.5)
    glVertex2d(0.5, -0.5)
    glEnd()
    glFlush()


def main():
    glutInit()
    glutCreateWindow("simple")
    glutDisplayFunc(display)
    glutMainLoop()

if __name__ == '__main__':
    main()
