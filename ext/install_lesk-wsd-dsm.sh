#!/bin/sh

JAVA_HOME=/user/vbasile/home/.local/lib/jdk1.7.0_79

# get the code
cd ext/
git clone https://github.com/pippokill/lesk-wsd-dsm.git
cd lesk-wsd-dsm

# compile
ant jar -Dplatforms.JDK_1.7.home=$JAVA_HOME

# Download babelNet 2.5.1 indexes and API
wget http://babelnet.org/data/2.5/babelnet-2.5-index-bundle.tar.bz2
tar -xjvf babelnet-2.5-index-bundle.tar.bz2
wget http://babelnet.org/data/2.5/BabelNet-API-2.5.tar.bz2
tar -xjvf BabelNet-API-2.5.tar.bz2
cp -r BabelNet-API-2.5/resources/* resources/
cp BabelNet-API-2.5/babelnet-api-2.5.jar lib/
mv lib/babelnet-api-1.1.1.jar lib/babelnet-api-1.1.1.jar.bak

# fix BabelNet configuration files
cat config/babelnet.var.properties  |  head -n -1 > config/babelnet.var.properties.new
echo "babelnet.dir="`pwd`/BabelNet-2.5 >> config/babelnet.var.properties.new
mv config/babelnet.var.properties.new config/babelnet.var.properties
cp BabelNet-API-2.5/config/babelnet.properties config/

# download the Word Space Model
wget https://dl.dropboxusercontent.com/u/66551436/termvectors_en.bin
mkdir -p resources/dsm
mv termvectors_en.bin resources/dsm/

# download WordNet 3.1
mkdir WordNet3.1
cd WordNet3.1
wget http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz
tar -xzvf wn3.1.dict.tar.gz
cd ..

# fix Wordnet configuration files
wndir=`pwd | sed 's/\//\\\\\//g'`
cat config/jlt.var.properties | sed -e "s/^jlt.wordnetPrefix.*/jlt.wordnetPrefix=$wndir\/WordNet3.1/" > config/jlt.var.properties.new
mv config/jlt.var.properties.new config/jlt.var.properties

# test
head -n 8 text/multilingual-all-words.en.plain > test.plain

./run.sh -i test.plain \
-o out.plain \
-cm doc \
-f plain \
-dsm ./resources/dsm/termvectors_en.bin \
-lang en \
-sc ./resources/sense/sense.freq \
-sf bn \
-c max \
-of plain \
-depth 1 

./run.sh -i test.plain -o out.plain -cm doc -f plain -dsm ./resources/dsm/termvectors_en.bin -lang en -sc ./resources/sense/sense.freq -sf bn -c max -of plain -depth 1 

