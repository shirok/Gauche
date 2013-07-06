#!/bin/sh

set -e

sudo apt-get install -qq -y zlib1g-dev libgdbm-dev slib

SOURCE=Gauche.tgz
wget -O $SOURCE http://practical-scheme.net/gauche/latest
tar xzvf $SOURCE
VERSION=`curl -s http://practical-scheme.net/gauche/latest/version`
SOURCE_DIR="Gauche-${VERSION}"
(cd $SOURCE_DIR; ./configure; make; sudo make install)
