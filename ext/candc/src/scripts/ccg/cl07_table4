#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table4

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "POS tagging development and test sets" | tee -a $TABLE/log

$BIN/pos --model $POS --input $GOLD/wsj00.raw --output $TABLE/wsj00.pos 2>> $TABLE/log
$BIN/pos --model $POS --input $GOLD/wsj23.raw --output $TABLE/wsj23.pos 2>> $TABLE/log

echo

function run(){
  SEC=$1;
  BETA=$2;
  K=$3;
  OTHER=$4;

  GOUT=$TABLE/$SEC.gold.$BETA.$K
  AOUT=$TABLE/$SEC.auto.$BETA.$K

  echo "super tagging WSJ $SEC with gold pos beta=$BETA k=$K" | tee -a $TABLE/log
  $BIN/msuper --model $SUPER --input $GOLD/wsj$SEC.pos --output $GOUT \
    --beta $BETA --dict_cutoff $K $OTHER 2>> $TABLE/log

  echo "evaluating super tagged WSJ $DATA set" | tee -a $TABLE/log
  $SCRIPTS/evalmulti $GOLD/wsj$SEC.stagged $SUPER $GOUT > $GOUT.eval

  echo "super tagging WSJ $SEC with auto pos beta=$BETA k=$K" | tee -a $TABLE/log
  $BIN/msuper --model $SUPER --input $TABLE/wsj$SEC.pos --output $AOUT \
    --beta $BETA --dict_cutoff $K $OTHER 2>> $TABLE/log

  echo "evaluating super tagged WSJ $DATA set" | tee -a $TABLE/log
  $SCRIPTS/evalmulti $GOLD/wsj$SEC.stagged $SUPER $AOUT > $AOUT.eval
}

# this first one is needed for numbers quoted in the text

run 00 0.075 20
run 00 0.030 20
run 00 0.010 20
run 00 0.005 20
run 00 0.001 150

for i in 0.075.20 0.030.20 0.010.20 0.005.20 0.001.150; do
  NCATS=`grep '^ncats per word' $TABLE/00.gold.$i.eval | sed 's/ncats per word =//'`
  GACCW=`grep '^ccg /w' $TABLE/00.gold.$i.eval | sed 's|ccg /w = ||; s/%.*//'`
  GACCS=`grep '^ccg /s' $TABLE/00.gold.$i.eval | sed 's|ccg /s = ||; s/%.*//'`
  AACCW=`grep '^ccg /w' $TABLE/00.auto.$i.eval | sed 's|ccg /w = ||; s/%.*//'`
  AACCS=`grep '^ccg /s' $TABLE/00.auto.$i.eval | sed 's|ccg /s = ||; s/%.*//'`
  echo $i $NCATS $GACCW $GACCS $AACCW $AACCS | tee -a $TABLE/results
done
