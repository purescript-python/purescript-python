cd passing
nvm install 12.16.1
nvm use 12.16.1
npm install -g purescript
npm install -g spago

# if travis linux, install 3.7. otherwise it exists
pyenv install 3.7 && pyenv global 3.7 || pyenv install 3.7.4 && pyenv global 3.7.4
pip3 install "purescripto>=0.8.0,<0.9.0"
spago test
