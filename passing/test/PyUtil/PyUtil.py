import sys


def setrecursionlimit(n: int):
    def action():
        sys.setrecursionlimit(n)

    return action


def foo(a):
    def res(b):
        return a + b

    return res
