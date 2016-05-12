#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *
from random import *

def init():
    """初始化函数"""
    glClearColor(1, 1, 1, 1)  # 设置白色背景
    glColor3f(1, 0, 0)  # 设置红色笔刷

    # 初始化视图
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(0, 50, 0, 50, -50, 50)  # 设置视景体
    glMatrixMode(GL_MODELVIEW)


def display():
    """显示回调函数"""
    vertices = ((0.0, 0.0, 0.0), (25.0, 50.0, 0.0), 
                (50.0, 0.0, 0.0), (25.0, 15.0, 25.0))  # 定义一个三角形（三维）
    p = [7.5, 5.0, 0.0]  # 初始化一个在三角形内部的点

    glClear(GL_COLOR_BUFFER_BIT)  # 清空帧缓存颜色

    # 开始绘制点
    glBegin(GL_POINTS)

    for _ in range(50000):
        j = int(random() * 4)  # 三个顶点中随机选择一个顶点

        # 计算中点
        p[0] = (p[0] + vertices[j][0]) / 2.0
        p[1] = (p[1] + vertices[j][1]) / 2.0
        p[2] = (p[2] + vertices[j][2]) / 2.0
        glVertex3fv(p)

    glEnd()
    glFlush()


def main():
    """主函数"""
    glutInit()
    glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB)  # 设置窗口的显示模式

    # 定义窗口属性
    glutInitWindowSize(500, 500)
    glutInitWindowPosition(0, 0)

    glutCreateWindow("sierpinski_2d")
    glutDisplayFunc(display)  # 显示回调函数

    init()  # 设置 OpenGL 状态

    glutMainLoop()  # 进入事件循环


if __name__ == '__main__':
    main()