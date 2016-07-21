#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table11

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "POS tagging test set" | tee -a $TABLE/log

$BIN/pos --model $POS --input $GOLD/wsj00.raw --output $TABLE/wsj00.pos 2>> $TABLE/log

function run(){
  MODEL=$1
  DECODER=$2
  FMT=$3
  LABEL=$4
  BETA=$5
  CUTOFF=$6
  START=$7
  INPUT=$8

  shift 8;

  PREFIX=$TABLE/00.a.$LABEL.$MODEL

  echo "parsing development set with $MODEL and $LABEL settings on gold POS" | tee -a $TABLE/log

  $BIN/parser --parser $WORK/$MODEL --super $SUPER --input $INPUT \
    --prefix $PREFIX --decoder $DECODER --printer deps \
    --force_words false --parser-maxsupercats $MAXCATS \
    --betas $BETA --dict_cutoffs $CUTOFF --start_level $START \
    --super-forward_beam_ratio $FWDBEAM --ifmt "$FMT" \
    $@

  echo "evaluating development set" | tee -a $TABLE/log

  $SCRIPTS/evaluate $GOLD/wsj00.stagged $GOLD/wsj00.ccgbank_deps $PREFIX.out \
    > $PREFIX.eval

  LP=`grep '^lp:' $PREFIX.eval | sed 's/^lp: *//; s/%.*//'`
  LR=`grep '^lr:' $PREFIX.eval | sed 's/^lr: *//; s/%.*//'`
  LF=`grep '^lf:' $PREFIX.eval | sed 's/^lf: *//; s/%.*//'`

  LSENT=`grep '^lsent:' $PREFIX.eval | sed 's/^lsent: *//; s/%.*//'`

  UP=`grep '^up:' $PREFIX.eval | sed 's/^up: *//; s/%.*//'`
  UR=`grep '^ur:' $PREFIX.eval | sed 's/^ur: *//; s/%.*//'`
  UF=`grep '^uf:' $PREFIX.eval | sed 's/^uf: *//; s/%.*//'`

  CATS=`grep '^cats:' $PREFIX.eval | sed 's/^cats: *//; s/%.*//'`

  COVER=`grep '^cover:' $PREFIX.eval | sed 's/^cover: *//; s/%.*//'`

  echo "$MODEL $LABEL $SET $LP $LR $LF $LSENT $UP $UR $UF $CATS $COVER" | tee -a $TABLE/results

  tail -22 $PREFIX.log | tee -a $TABLE/results
}

BETA="0.075,0.03,0.01,0.005,0.001"
CUTOFF="20,20,20,20,150"
START="0"

BETA_REV=$BETA
CUTOFF_REV=$CUTOFF
START_REV="3"

run $HYBRID deps '%w|%p \n' normal $BETA $CUTOFF $START $TABLE/wsj00.pos
run $HYBRID deps '%w|%p \n' reverse $BETA_REV $CUTOFF_REV $START_REV $TABLE/wsj00.pos
