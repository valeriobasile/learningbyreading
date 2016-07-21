#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table9

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "POS tagging test set" | tee -a $TABLE/log

$BIN/pos --model $POS --input $GOLD/wsj23.raw --output $TABLE/wsj23.pos 2>> $TABLE/log

echo

function run(){
  MODEL=$1
  DECODER=$2
  INPUT=$3
  SET=$4

  PREFIX=$TABLE/23.$SET.$MODEL

  echo "parsing $INPUT ($SET) with $MODEL" | tee -a $TABLE/log

  $BIN/parser --parser $WORK/$MODEL --super $SUPER --input $INPUT \
    --prefix $PREFIX --decoder $DECODER --printer deps \
    --force_words false --parser-maxsupercats $MAXCATS \
    --betas $BETA --dict_cutoffs $CUTOFF \
    --super-forward_beam_ratio $FWDBEAM \
    $OTHER

  echo "evaluating $INPUT ($SET)" | tee -a $TABLE/log

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

  echo "$MODEL $SET $LP $LR $LF $LSENT $UP $UR $UF $CATS $COVER" | tee -a $TABLE/results
}

run $HYBRID deps $GOLD/wsj23.pos g
run $HYBRID deps $TABLE/wsj23.pos a
