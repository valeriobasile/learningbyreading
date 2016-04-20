#!/bin/sh
CANDCUSER=
CANDCPASSWORD=

mkdir -p candc
svn co --username $CANDCUSER --password $CANDCPASSWORD http://svn.ask.it.usyd.edu.au/candc/trunk candc
cd candc
ln -s Makefile.unix Makefile
BASEDIR=`pwd`

mkdir -p ext
cd ext
wget -c "http://downloads.sourceforge.net/project/gsoap2/gSOAP/gsoap_2.8.16.zip"
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

wget --http-user=$CANDCUSER --http-passwd=$CANDCPASSWORD http://svn.ask.it.usyd.edu.au/download/candc/models-1.02.tbz2
tar -xjvf models-1.02.tbz2
