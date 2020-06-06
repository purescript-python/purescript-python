import sys

def setrecursionlimit(n: int):
    def action():
        sys.setrecursionlimit(n)

    return action

def getrecursionlimit():
    return sys.getrecursionlimit()