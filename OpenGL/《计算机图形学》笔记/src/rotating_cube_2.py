#!/usr/bin/env python3
# -*- coding:utf-8 -*-


from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *


vertices = ((-1, -1, 1), (-1, 1, 1),
            (1, 1, 1), (1, -1, 1),
            (-1, -1, -1), (-1, 1, -1),
            (1, 1, -1), (1, -1, -1))
colors = ((0, 0, 0), (1, 0, 0),
          (1, 1, 0), (0, 1, 0),
          (0, 0, 1), (1, 0, 1),
          (1, 1, 1), (0, 1, 1))
vertex_list = (0, 3, 2, 1,
               2, 3, 7, 6,
               4, 5, 6, 7,
               0, 1, 5, 4,
               1, 2, 6, 5,
               0, 4, 7, 3)
theta = [0] * 3
axis = 0


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
    glLoadIdentity()
    glRotatef(theta[0], 1, 0, 0)
    glRotatef(theta[1], 0, 1, 0)
    glRotatef(theta[2], 0, 0, 1)

    glBegin(GL_QUADS)

    color_cube()

    glEnd()

    glFlush()
    glutSwapBuffers()


def quad(i, j, k, l):
    """绘制平面"""
    glColor3fv(colors[i])
    glVertex3fv(vertices[i])

    glColor3fv(colors[j])
    glVertex3fv(vertices[j])

    glColor3fv(colors[k])
    glVertex3fv(vertices[k])

    glColor3fv(colors[l])
    glVertex3fv(vertices[l])


def color_cube():
    """绘制立方体"""
    for i in range(0, 24, 4):
        quad(vertex_list[i],
             vertex_list[i + 1],
             vertex_list[i + 2],
             vertex_list[i + 3])


def keyboard(key, x, y):
    """键盘回调函数"""
    global axis

    if key == b'x':
        axis = 0
    elif key == b'y':
        axis = 1
    elif key == b'z':
        axis = 2
    elif key == b'q':
        exit(0)


def spin_cube():
    """空闲回调函数"""
    global axis

    theta[axis] += 0.5
    theta[axis] = theta[axis] - 360 if theta[axis] > 360 else theta[axis]
    glutPostRedisplay()


def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH)

    glutCreateWindow("rotating_cube")

    glutDisplayFunc(display)
    glutIdleFunc(spin_cube)
    glutKeyboardFunc(keyboard)

    init()

    glutMainLoop()


if __name__ == '__main__':
    main()