from base64 import b32decode
import sys
import os

def auth():
    AUTH = os.environ['RELEASE_AUTH']
    sys.stdout.write(b32decode(AUTH.encode('utf8')).decode('utf8').strip())

def platform():
    from distutils.util import get_platform
    sys.stdout.write(get_platform())

{
    'auth': auth,
    'plat': platform
}[sys.argv[1]]()
