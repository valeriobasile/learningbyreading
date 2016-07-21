#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table8

HYBRID=model_hybrid

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "POS tagging development set" | tee -a $TABLE/log

$BIN/pos --model $POS --input $GOLD/wsj00.raw --output $OUT/wsj00.pos 2>> $TABLE/log

echo

function run(){
  MODEL=$1
  DECODER=$2
  OTHER=$3

  echo "parsing development set with $MODEL on gold POS" | tee -a $TABLE/log

  $BIN/parser --parser $WORK/$MODEL --super $SUPER --input $GOLD/wsj00.pos \
    --prefix $TABLE/00.g.$MODEL --decoder $DECODER --printer deps \
    --force_words false --parser-maxsupercats $MAXCATS \
    --betas $BETA --dict_cutoffs $CUTOFF \
    --super-forward_beam_ratio $FWDBEAM \
    $OTHER

  echo "evaluating development set" | tee -a $TABLE/log

  $SCRIPTS/evaluate -r $GOLD/wsj00.stagged $GOLD/wsj00.ccgbank_deps $TABLE/00.g.$MODEL.out \
    > $TABLE/00.g.$MODEL.eval

  echo "parsing development set with $MODEL on auto POS" | tee -a $TABLE/log

  $BIN/parser --parser $WORK/$MODEL --super $SUPER --input $OUT/wsj00.pos \
    --prefix $TABLE/00.a.$MODEL --decoder $DECODER --printer deps \
    --force_words false  --parser-maxsupercats $MAXCATS \
    --betas $BETA --dict_cutoffs $CUTOFF \
    --super-forward_beam_ratio $FWDBEAM \
    $OTHER

  echo "evaluating development set" | tee -a $TABLE/log

  $SCRIPTS/evaluate -r $GOLD/wsj00.stagged $GOLD/wsj00.ccgbank_deps $TABLE/00.a.$MODEL.out \
    > $TABLE/00.a.$MODEL.eval

  (echo "Gold POS tag evaluation"; echo) >> $TABLE/results
  cat $TABLE/00.a.$MODEL.eval >> $TABLE/results

  (echo "Automatically assigned POS tag evaluation"; echo) >> $TABLE/results
  cat $TABLE/00.a.$MODEL.eval >> $TABLE/results
}

run $HYBRID deps ""
