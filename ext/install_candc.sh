#!/bin/sh
tar -xjvf candc.tar.bz2
cd candc
ln -s Makefile.unix Makefile
BASEDIR=`pwd`

cd ext
unzip gsoap_2.8.16.zip
cd gsoap-2.8
./configure --prefix=$BASEDIR/ext
make
make install

cd ../../
make
make bin/t
make bin/boxer
make soap

tar -xjvf models-1.02.tbz2
