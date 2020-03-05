import sys
import os

def platform():
    from distutils.util import get_platform
    sys.stdout.write(get_platform())

{
    'plat': platform
}[sys.argv[1]]()
