#!/bin/sh

git clone https://github.com/Noahs-ARK/semafor.git
cd semafor
wget http://www.ark.cs.cmu.edu/SEMAFOR/semafor_malt_model_20121129.tar.gz
tar -xzvf semafor_malt_model_20121129.tar.gz
cat bin/config.sh | sed -e "s/\/models\//\/semafor\//g" > bin/config.sh_
mv bin/config.sh_ bin/config.sh
mvn package
