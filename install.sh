echo "installing"
stack build
export DIST_FILE="`stack path --dist-dir`/build/psdiana/psdiana"
cp $DIST_FILE ~/.local/bin
export RELEASE_TAG="`python travis-env.py plat`"
export ZIP_FILE="psdiana-`python travis-env.py plat`.zip"
zip -r $ZIP_FILE . -x "*.git*" "*.stack-work*" "passing/*" "src/" "app/" ".gitignore" "*.py"
