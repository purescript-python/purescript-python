def assertMsg(cond):
    def ap_msg(msg):
        def ap():
            assert cond, msg
            return ()
        return ap
    return ap_msg

def assert_(cond):
    def ap():
        assert cond
        return ()
    return ap

def repr(s, *, r=repr):
    return lambda: r(s)

unsafeEval = eval

def eval(s, *, r=eval):
    return lambda: r(s)


def error(s):
    raise Exception(s)
