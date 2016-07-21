#!/bin/bash

SET=$1
DATASETS=working/rte/datasets

convert()
{
    echo $1
    cat $1 \
    | tr "\n" " " \
    | tr "\t" " " \
    | tr "\r" " " \
    | tr -s " " \
    | sed "s/<\/t>/@/g" \
    | sed "s/<\/h>/@cd ..@/g" \
    | tr "@" "\n" \
    | sed 's/id="\([^ ]*\)"/  @mkdir -p \1; cd \1@/' \
    | tr "@" "\n" \
    | tr "@" "\n" \
    | sed 's/value="FALSE"/@echo informative > gold.txt@/' \
    | sed 's/value="TRUE"/@echo entailed > gold.txt@/' \
    | sed 's/entailment="NO"/@echo informative > gold.txt@/' \
    | sed 's/entailment="YES"/@echo entailment > gold.txt@/' \
    | sed 's/task="\([A-Z]*\)">/@echo \1 > task.txt@/' \
    | sed 's/task="\([A-Z]*\)"/@echo \1 > task.txt@/' \
    | sed 's/word1="\(.*\)" word2/word2/' \
    | sed 's/word2="\(.*\)">//' \
    | tr "@" "\n" \
    | sed 's/"/\\\"/g' \
    | sed 's/<t>\(.*\)/@echo "\1" > t@/' \
    | tr "@" "\n" \
    | sed 's/<h>\(.*\)/@echo "\1" > h@/' \
    | tr "@" "\n" \
    | sed "s/\`/\\\\\`/g" \
    | sed "s/&apos;/'/g" \
    | sed "s/&quot;/ /g" \
    | sed "s/&amp;/ \& /g" \
    | grep -v "<pair" \
    | grep -v "<\/pair" \
    | grep -v "length=" \
    | grep -v "^$" \
    | grep -v "^ $" \
    >> working/run.tmp
}

rte()
{
    if [ -e ${DATASETS}/${SET}.xml ]; then
	echo "pushd ." > working/run.tmp
	echo "mkdir -p working/rte/${SET}" >> working/run.tmp
	echo "cd working/rte/${SET}" >> working/run.tmp
	convert "$DATASETS/${SET}.xml" 
	echo "popd" >> working/run.tmp
	chmod 700 working/run.tmp
	./working/run.tmp
#	rm -f working/run.tmp
    else
	echo "ERROR: file ${DATASETS}/${SET}.xml does not exist"
	ls ${DATASETS}/*.xml
    fi
}

if [ -d ${DATASETS} ]; then
    echo "Building ${SET}"
    rte ${SET}
    exit 0
else
    echo "ERROR: directory ${DATASETS} does not exist"
    exit 1
fi




