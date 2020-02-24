echo "==================setting travis release uploading environment================"
export RELEASE_AUTH_KEY="`python travis-env.py auth`"
export DIST_FILE="`stack path --dist-dir`/build/pspy-blueprint/pspy-blueprint"
cp $DIST_FILE ./pspy-blueprint
export ZIP_FILE="pspy-blueprint-`python travis-env.py plat`.zip"
zip -r $ZIP_FILE . -x "*.git*" "*.stack-work*"
