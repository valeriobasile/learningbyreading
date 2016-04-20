#!/bin/sh

wget http://ixa2.si.ehu.es/ukb/ukb_2.2.tgz
tar -xzvf ukb_2.2.tgz
cd ukb
wget http://ixa2.si.ehu.es/ukb/lkb_sources.tar.bz2
tar -xjvf lkb_sources.tar.bz2
bin/compile_kb -o wn30.bin lkb_sources/30/wnet30_rels.txt

