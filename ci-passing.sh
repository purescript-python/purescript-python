cd passing

nvm install 12.16.1
nvm use 12.16.1


# this allows failure
pyenv global 3.7
pip3 install "purescripto>=0.8.0,<0.9.0"

spago test
