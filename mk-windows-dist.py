from distutils.dir_util import copy_tree
from distutils.file_util import copy_file
from subprocess import check_output
from pathlib import Path

exe_name = "pspy-blueprint"
out_dir = "windows-libs"
Path(out_dir).mkdir(exist_ok=True)
copy_tree("licenses", out_dir)
builddir = check_output(['stack', 'path', '--dist-dir']).decode('utf8')

exe_path = Path(builddir.strip()) / "build" / exe_name / exe_name
exe_path = exe_path.with_suffix(".exe")
print(str(exe_path))
with exe_path.open('rb') as f:
    exe_bytes = f.read()

with (Path(out_dir) / (exe_name + '.exe')).open('wb') as f:
    f.write(exe_bytes)
