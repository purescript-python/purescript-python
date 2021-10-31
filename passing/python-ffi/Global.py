import math
import urllib.parse


nan = math.nan
isNaN = math.isnan


infinity = math.inf
isFinite = math.isfinite


def readInt(radix):
    def ap(n):
        try:
            return float(int(n, radix))
        except ValueError:
            try:
                return float(int(str(int(float(n))), radix))
            except ValueError:
                return math.nan
            return math.nan

    return ap


readFloat = float


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


def formatNumber(fmt):
    def ap(fail, succ, digits, n):
        try:
            return succ(fmt(digits)(n))
        except Exception as e:
            return fail(str(e))

    return ap


_toFixed = formatNumber(unsafeToFixed)
_toExponential = formatNumber(unsafeToExponential)
_toPrecision = formatNumber(unsafeToPrecision)


def encdecURI(encdec):
    def ap(fail, succ, s):
        try:
            return succ(encdec(s))
        except Exception as e:
            return fail(str(e))

    return ap


def decodeURI(s):
    return urllib.parse.unquote(s, errors="strict")


def encodeURI(s):
    return urllib.parse.quote(s, safe="~@#$&()*!+=:;,.?/'")


def decodeURIComponent(s):
    return urllib.parse.unquote(s, errors="strict")


def encodeURIComponent(s):
    return urllib.parse.quote(s, safe="~()*!.'")


_decodeURI = encdecURI(decodeURI)
_encodeURI = encdecURI(encodeURI)
_decodeURIComponent = encdecURI(decodeURIComponent)
_encodeURIComponent = encdecURI(encodeURIComponent)
