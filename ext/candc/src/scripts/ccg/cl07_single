#!/bin/bash

BIN=bin
SCRIPTS=src/scripts/ccg
DATA=src/data/ccg

WORK=working/ccg
GOLD=$WORK/gold
GEN=$WORK/generated
POS=$WORK/pos
SUPER=$WORK/super
OUT=$WORK/cl07
TABLE=$OUT/single

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "POS tagging development and test sets" | tee -a $TABLE/log

$BIN/pos --model $POS --input $GOLD/wsj00.raw --output $TABLE/wsj00.pos 2>> $TABLE/log
$BIN/pos --model $POS --input $GOLD/wsj23.raw --output $TABLE/wsj23.pos 2>> $TABLE/log

echo

function run(){
  SEC=$1;
  OTHER=$4;

  GOUT=$TABLE/$SEC.gold
  AOUT=$TABLE/$SEC.auto

  echo "super tagging WSJ $SEC with gold pos" | tee -a $TABLE/log
  $BIN/super --model $SUPER --input $GOLD/wsj$SEC.pos --output $GOUT \
    --ofmt '%w\t%p\t%w\t%s\n\n\n' $OTHER 2>> $TABLE/log

  echo "evaluating super tagged WSJ $DATA set" | tee -a $TABLE/log
  $SCRIPTS/evalmulti $GOLD/wsj$SEC.stagged $SUPER $GOUT > $GOUT.eval

  echo "super tagging WSJ $SEC with auto pos" | tee -a $TABLE/log
  $BIN/super --model $SUPER --input $TABLE/wsj$SEC.pos --output $AOUT \
    --ofmt '%w\t%p\t%w\t%s\n\n\n' $OTHER 2>> $TABLE/log

  echo "evaluating super tagged WSJ $DATA set" | tee -a $TABLE/log
  $SCRIPTS/evalmulti $GOLD/wsj$SEC.stagged $SUPER $AOUT > $AOUT.eval
}

# this first one is needed for numbers quoted in the text

run 00

GACCW=`grep '^ccg /w' $TABLE/00.gold.eval | sed 's|ccg /w = ||; s/%.*//'`
GACCS=`grep '^ccg /s' $TABLE/00.gold.eval | sed 's|ccg /s = ||; s/%.*//'`
AACCW=`grep '^ccg /w' $TABLE/00.auto.eval | sed 's|ccg /w = ||; s/%.*//'`
AACCS=`grep '^ccg /s' $TABLE/00.auto.eval | sed 's|ccg /s = ||; s/%.*//'`

echo 'gold:' $GACCW $GACCS | tee -a $TABLE/results
echo 'auto:' $AACCW $AACCS | tee -a $TABLE/results

