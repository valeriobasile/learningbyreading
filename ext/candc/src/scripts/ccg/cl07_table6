#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table6

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "POS tagging development set" | tee -a $TABLE/log

$BIN/pos --model $POS --input $GOLD/wsj00.raw --output $TABLE/wsj00.pos 2>> $TABLE/log

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

  $SCRIPTS/evaluate $GOLD/wsj00.stagged $GOLD/wsj00.ccgbank_deps $TABLE/00.g.$MODEL.out \
    > $TABLE/00.g.$MODEL.eval

  echo "parsing development set with $MODEL on auto POS" | tee -a $TABLE/log

  $BIN/parser --parser $WORK/$MODEL --super $SUPER --input $TABLE/wsj00.pos \
    --prefix $TABLE/00.a.$MODEL --decoder $DECODER --printer deps \
    --force_words false  --parser-maxsupercats $MAXCATS \
    --betas $BETA --dict_cutoffs $CUTOFF \
    --super-forward_beam_ratio $FWDBEAM \
    $OTHER

  echo "evaluating development set" | tee -a $TABLE/log

  $SCRIPTS/evaluate $GOLD/wsj00.stagged $GOLD/wsj00.ccgbank_deps $TABLE/00.a.$MODEL.out \
    > $TABLE/00.a.$MODEL.eval

  LP=`grep '^lp:' $TABLE/00.g.$MODEL.eval | sed 's/^lp: *//; s/%.*//'`
  LR=`grep '^lr:' $TABLE/00.g.$MODEL.eval | sed 's/^lr: *//; s/%.*//'`
  LF=`grep '^lf:' $TABLE/00.g.$MODEL.eval | sed 's/^lf: *//; s/%.*//'`

  ALF=`grep '^lf:' $TABLE/00.a.$MODEL.eval | sed 's/^lf: *//; s/%.*//'`
  LSENT=`grep '^lsent:' $TABLE/00.g.$MODEL.eval | sed 's/^lsent: *//; s/%.*//'`

  UP=`grep '^up:' $TABLE/00.g.$MODEL.eval | sed 's/^up: *//; s/%.*//'`
  UR=`grep '^ur:' $TABLE/00.g.$MODEL.eval | sed 's/^ur: *//; s/%.*//'`
  UF=`grep '^uf:' $TABLE/00.g.$MODEL.eval | sed 's/^uf: *//; s/%.*//'`

  CATS=`grep '^cats:' $TABLE/00.g.$MODEL.eval | sed 's/^cats: *//; s/%.*//'`

  COVER=`grep '^cover:' $TABLE/00.g.$MODEL.eval | sed 's/^cover: *//; s/%.*//'`

  echo "$MODEL $LP $LR $LF $ALF $LSENT $UP $UR $UF $CATS $COVER" | tee -a $TABLE/results
}

run $DEPS deps "--parser-seen_rules false --parser-eisner_nf false"
run $DERIVS derivs ""
run $DERIVS_REV derivs ""
