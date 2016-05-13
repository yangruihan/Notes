#!/usr/bin/env python3
# -*- coding:utf-8 -*-


from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *


vertices = ((-1, -1, 1), (-1, 1, 1),
            (1, 1, 1), (1, -1, 1),
            (-1, -1, -1), (-1, 1, -1),
            (1, 1, -1), (1, -1, -1))


def init():
    """初始化函数"""
    glClearColor(1, 1, 1, 1)
    glEnable(GL_DEPTH_TEST)

    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()

    glOrtho(-2, 2, -2, 2, -2, 2)
    glMatrixMode(GL_MODELVIEW)
    glutPostRedisplay()


def display():
    """显示回调函数"""
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

    glBegin(GL_QUADS)

    glColor3f(1, 0, 0)
    glVertex3fv(vertices[0])
    glVertex3fv(vertices[3])
    glVertex3fv(vertices[2])
    glVertex3fv(vertices[1])

    glColor3f(0, 1, 0)
    glVertex3fv(vertices[2])
    glVertex3fv(vertices[3])
    glVertex3fv(vertices[7])
    glVertex3fv(vertices[6])

    glColor3f(0, 0, 1)
    glVertex3fv(vertices[1])
    glVertex3fv(vertices[0])
    glVertex3fv(vertices[4])
    glVertex3fv(vertices[5])

    glColor3f(1, 1, 0)
    glVertex3fv(vertices[1])
    glVertex3fv(vertices[2])
    glVertex3fv(vertices[5])
    glVertex3fv(vertices[6])

    glColor3f(1, 0, 1)
    glVertex3fv(vertices[0])
    glVertex3fv(vertices[3])
    glVertex3fv(vertices[4])
    glVertex3fv(vertices[7])

    glColor3f(0, 1, 1)
    glVertex3fv(vertices[5])
    glVertex3fv(vertices[4])
    glVertex3fv(vertices[7])
    glVertex3fv(vertices[6])
    glEnd()

    glFlush()
    glutSwapBuffers()


def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH)

    glutCreateWindow("rotating_cube")

    glutDisplayFunc(display)

    init()

    glutMainLoop()


if __name__ == '__main__':
    main()