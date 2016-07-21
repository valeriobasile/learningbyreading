#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table12

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

function run(){
  MODEL=$1
  DECODER=$2
  OTHER=$3

  echo "parsing test set with $MODEL on gold POS" | tee -a $TABLE/log

  $BIN/parser --parser $WORK/$MODEL --super $SUPER --input $GOLD/wsj23.pos \
    --prefix $TABLE/23.g.$MODEL --decoder $DECODER --printer deps \
    --force_words false --parser-maxsupercats $MAXCATS \
    --betas $BETA --dict_cutoffs $CUTOFF \
    --super-forward_beam_ratio $FWDBEAM \
    $OTHER

  tail -3 $TABLE/23.g.$MODEL.log >> $TABLE/results
}

run $HYBRID deps ""
