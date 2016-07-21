#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table16

TESTS=src/tests
DEPBANK=$TESTS/depbank560
GREVAL=$WORK/greval
INPUT=depbank560.retok

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "changing brackets in DepBank560 set" | tee -a $TABLE/log

# uncomment to use with the Penn Treebank bracket representation
# sed 's/(/-LRB-/g; s/)/-RRB-/g; s/{/-LCB-/g; s/}/-RCB-/g; s/\[/-LSB-/g; s/\]/-RSB-/g' \
# $DEPBANK/$INPUT.txt > $TABLE/$INPUT.txt

cat $DEPBANK/$INPUT.txt > $TABLE/$INPUT.txt

echo "POS tagging DepBank560 set" | tee -a $TABLE/log

$BIN/pos --model $POS --input $TABLE/$INPUT.txt \
  --output $TABLE/$INPUT.autopos 2>> $TABLE/log

echo

function run(){
  MODEL=$1
  DECODER=$2
  OTHER=$3

  echo "parsing DepBank560 set with $MODEL" | tee -a $TABLE/log

  $BIN/parser --parser $WORK/$MODEL --super $SUPER --input $TABLE/$INPUT.autopos \
    --prefix $TABLE/$INPUT.autopos --decoder $DECODER --printer grs \
    --force_words true  --parser-maxsupercats $MAXCATS \
    --betas $BETA --dict_cutoffs $CUTOFF \
    --super-forward_beam_ratio $FWDBEAM \
    $OTHER

  echo "running post processing script" | tee -a $TABLE/log

  $SCRIPTS/grs2depbank --rasp-parse $TABLE/$INPUT.autopos.out > $TABLE/$INPUT.autopos.parses

  cp $GREVAL/ccg/$INPUT.autopos.grtext $TABLE

  $GREVAL/eval_system.sh -t $TABLE/$INPUT.autopos -c $GREVAL/DepBank/parc560/sents-tst.gr \
    > $TABLE/$INPUT.autopos.output.log

  cp $TABLE/$INPUT.autopos.output $TABLE/results
}

run $HYBRID deps ""


