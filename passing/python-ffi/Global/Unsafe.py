import json
import urllib.parse
import re


def unsafeStringify(x):
    return json.dumps(x)


def unsafeToFixed(digits):
    def n_(n):
        f = r"{:0." + str(digits) + r"f}"
        return f.format(n)

    return n_


def unsafeToExponential(digits):
    def n_(n):
        f = r"{:." + str(digits) + r"e}"
        return f.format(n).replace("e-0", "e-").replace("e+0", "e+")

    return n_


def unsafeToPrecision(digits):
    def n_(n):
        raise NotImplementedError()

    return n_


def unsafeDecodeURI(s):
    return urllib.parse.unquote(s, errors="strict")


def unsafeEncodeURI(s):
    return urllib.parse.quote(s, safe="~@#$&()*!+=:;,.?/'")

def unsafeDecodeURIComponent(s):
    return urllib.parse.unquote(s, errors="strict")


def unsafeEncodeURIComponent(s):
    return urllib.parse.quote(s, safe="~()*!.'")
