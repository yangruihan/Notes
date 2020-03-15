#!/usr/bin/env python3
# -*- coding:utf-8 -*-

from implementation import *

example_graph = SimpleGraph()
example_graph.edges = {
    'A': ['B'],
    'B': ['A', 'C', 'D'],
    'C': ['A'],
    'D': ['E', 'A'],
    'E': ['B']
}


def breadth_first_search_1(graph: SimpleGraph, start):
    frontier = Queue()
    frontier.put(start)
    visited = {}
    visited[start] = True

    while not frontier.empty():
        current = frontier.get()
        print("Visiting %r" % current)
        for next in graph.neighbors(current):
            if next not in visited:
                frontier.put(next)
                visited[next] = True


def breadth_first_search_2(graph: SquareGrid, start):
    frontier = Queue()
    frontier.put(start)
    came_from = {}
    came_from[start] = None

    while not frontier.empty():
        current = frontier.get()
        for next in graph.neighbors(current):
            if next not in came_from:
                frontier.put(next)
                came_from[next] = current

    return came_from


DIAGRAM1_WALLS = []
for i in range(3, 5):
    for j in range(3, 12):
        DIAGRAM1_WALLS.append((i, j))

for i in range(13, 15):
    for j in range(4, 15):
        DIAGRAM1_WALLS.append((i, j))

for i in range(21, 23):
    for j in range(0, 7):
        DIAGRAM1_WALLS.append((i, j))

for i in range(23, 26):
    for j in range(5, 7):
        DIAGRAM1_WALLS.append((i, j))


def draw_grid(g: SquareGrid, width: int, point_to: list, start: tuple):
    for j in range(g.height):
        for i in range(g.width):
            if (i, j) == start:
                print('A' + ' ' * (width - 1), end='')
            elif not g.passable((i, j)):
                print('#' * width, end='')
            else:
                came_from = point_to[(i, j)]
                space = ' ' * (width - 1)
                if came_from == (i - 1, j):
                    print('<' + space, end='')
                elif came_from == (i + 1, j):
                    print('>' + space, end='')
                elif came_from == (i, j - 1):
                    print('^' + space, end='')
                else:
                    print('V' + space, end='')
        print()

# breadth_first_search_1(example_graph, 'A')


g = SquareGrid(30, 15)
g.walls = DIAGRAM1_WALLS
parents = breadth_first_search_2(g, (8, 7))
draw_grid(g, width=2, point_to=parents, start=(8, 7))
