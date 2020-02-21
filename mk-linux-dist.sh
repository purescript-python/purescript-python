# NOTE: This packaging script tested only on Arch/Manjaro Linux.
python mk-linux-dist.py
cd linux-libs
chrpath -r "./" pspy-blueprint
cd ..
zip -r pspy-blueprint-linux.zip linux-libs
rm -rf linux-libs
