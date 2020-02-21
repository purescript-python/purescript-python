"""
NOTE: This packaging script tested only on Arch/Manjaro Linux.
"""
from subprocess import check_output, check_call
from distutils.dir_util import copy_tree
from pprint import pprint
from pathlib import Path
import re
import os

exe_name = "pspy-blueprint"
out_dir = "linux-libs"

Path(out_dir).mkdir(exist_ok=True)
copy_tree("licenses", out_dir)

check_call(['stack', 'build'])


def copy_file(src, dst):
    print('reading '+src)
    with open(src, 'rb') as f:
        b = f.read()

    print('writing '+dst)
    with open(dst, 'wb') as f:
        f.write(b)

path = check_output(['which', exe_name]).decode('utf8')
sos = check_output(['ldd', path.strip()]).decode('utf8')

pat = re.compile("(\S+)\s+=>\s+(\S+)")
libc_pat = re.compile("libc\.so\..")

for dst, src in pat.findall(sos):
    dst = Path(dst).name
    if libc_pat.match(dst):
        continue

    dst = os.path.join(out_dir, dst)
    copy_file(src, dst)

builddir = check_output(["stack", "path", "--dist-dir"]).decode('utf8').strip()

copy_file(
    os.path.join(builddir, "build", exe_name, exe_name),
    os.path.join(out_dir, exe_name)
)


os.chdir(out_dir)
check_call(["chrpath", "-r", "./", exe_name])
copy_tree("./licenses", out_dir)
