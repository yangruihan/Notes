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


def mymenu(value):
    if value == 1:
        glClearColor(1, 1, 1, 1)
        glutPostRedisplay()

    if value == 2:
        exit(0)


def main():
    glutInit()
    glutCreateWindow("simple")
    glutDisplayFunc(display)

    menu_id = glutCreateMenu(mymenu)
    glutAddMenuEntry("Clear Screen", 1)
    glutAddMenuEntry("Exit", 2)
    glutAttachMenu(GLUT_LEFT_BUTTON)

    glutMainLoop()

if __name__ == '__main__':
    main()
