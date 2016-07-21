#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table10

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "POS tagging test set" | tee -a $TABLE/log

$BIN/pos --model $POS --input $GOLD/wsj23.raw --output $TABLE/wsj23.pos 2>> $TABLE/log

function run(){
  MODEL=$1
  DECODER=$2
  FMT=$3
  LABEL=$4
  BETA=$5
  CUTOFF=$6
  START=$7
  SET=$8
  INPUT=$9

  shift 9;

  PREFIX=$TABLE/23.$SET.$LABEL.$MODEL

  echo "parsing test set with $MODEL and $LABEL settings on $SET POS" | tee -a $TABLE/log

  $BIN/parser --parser $WORK/$MODEL --super $SUPER --input $INPUT \
    --prefix $PREFIX --decoder $DECODER --printer deps \
    --force_words false --parser-maxsupercats $MAXCATS \
    --betas $BETA --dict_cutoffs $CUTOFF --start_level $START \
    --super-forward_beam_ratio $FWDBEAM --ifmt "$FMT" \
    $@

  echo "evaluating $SET set" | tee -a $TABLE/log

  $SCRIPTS/evaluate $GOLD/wsj23.stagged $GOLD/wsj23.ccgbank_deps $PREFIX.out \
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

  (
  echo "$MODEL $LABEL $SET $LP $LR $LF $LSENT $UP $UR $UF $CATS $COVER";
  tail -3 $PREFIX.log;
  echo
  ) | tee -a $TABLE/results
}

BETA_NORM="0.075,0.03,0.01,0.005,0.001"
CUTOFF_NORM="20,20,20,20,150"
START_NORM="0"

BETA_REV=$BETA_NORM
CUTOFF_REV=$CUTOFF_NORM
START_REV="3"

run $HYBRID deps '%w|%p \n' normal $BETA_NORM $CUTOFF_NORM $START_NORM gold $GOLD/wsj23.pos
run $HYBRID deps '%w|%p \n' reverse $BETA_REV $CUTOFF_REV $START_REV gold $GOLD/wsj23.pos
run $HYBRID deps '%w|%p \n' single "0.075" "20" "0" gold $GOLD/wsj23.pos
run $HYBRID deps '%w|%p|%s \n' oracle "1.0" "1" "0" gold $GOLD/wsj23.stagged --oracle

run $HYBRID deps '%w|%p \n' normal $BETA_NORM $CUTOFF_NORM $START_NORM auto $TABLE/wsj23.pos
run $HYBRID deps '%w|%p \n' reverse $BETA_REV $CUTOFF_REV $START_REV auto $TABLE/wsj23.pos
run $HYBRID deps '%w|%p \n' single "0.075" "20" "0" auto $TABLE/wsj23.pos
