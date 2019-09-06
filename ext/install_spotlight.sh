#!/bin/sh

mkdir -p spotlight
cd spotlight
wget https://gitlab.tubit.tu-berlin.de/freecookie/text-processing/blob/master/spotlight-server/dbpedia-spotlight-latest.jar
wget https://downloads.sourceforge.net/project/dbpedia-spotlight/2016-10/en/model/en.tar.gz
tar -xzvf en.tar.gz
