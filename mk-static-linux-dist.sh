python mk-static-linux-dist.py
cd linuxlibs
chrpath -r "./" pspy-blueprint
cd ..
zip -r pspy-blueprint-linux.zip linuxlibs
rm -rf linuxlibs
