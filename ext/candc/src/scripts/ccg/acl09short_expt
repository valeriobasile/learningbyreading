#!/bin/bash

NAME=$1
COND=$2
W=working
E=$W/acl09short
D=src/data/ccg2ptb
SCRIPT=src/scripts/ccg
FEATS=/n/nlp6/u1/repos/candc/features
FW=$FEATS/working+SWT
MODEL=$FW/model_derivs+STW

# location of TGrep2 (available from http://tedlab.mit.edu/~dr/TGrep2/)
TGREP2=working/TGrep2

# location of EvalB (available from http://nlp.cs.nyu.edu/evalb/)
# we used version http://nlp.cs.nyu.edu/evalb/EVALB20060307.tgz
EVALB=working/EVALB

# location of the Berkeley parser (available from http://code.google.com/p/berkeleyparser/)
BERK=working/berkeley/

# cat ~ltrg/corpora/TreeBank3/parsed/mrg/wsj/23/wsj_23* | sed 's/^(/(TOP/g' > wsj23.txt

# $TGREP/tgrep2 -p working/wsj23.txt wsj23.t2c
# $TGREP/tgrep2 -c wsj23.t2c -w '/.*/' > working/orig23.txt

# $SCRIPT/extract_ccg2ptb_gold $W/orig00.txt $E/ccg00.txt $E/join00
# $SCRIPT/extract_ccg2ptb_gold $W/orig23.txt $E/ccg23.txt $E/join23
# $SCRIPT/extract_ccg2ptb_gold $W/orig02-21.txt $E/ccg02-21.txt $E/join02-21

# bin/pos --model models/pos --input $E/join00.txt --output $E/join00.auto
# bin/pos --model models/pos --input $E/join23.txt --output $E/join23.auto

# java -Xms900m -Xmx900m -jar $BERK/berkeleyParser.jar -gr $BERK/eng_sm5.gr < join23.qtxt \
#   | sed 's/^(/(TOP/' > join23.berk.a
# java -Xms900m -Xmx900m -jar $BERK/berkeleyParser.jar -gr $BERK/eng_sm5.gr -accurate < join23.qtxt \
#   | sed 's/^(/(TOP/' > join23.bacc.a

rm -rf $E/$NAME

$SCRIPT/ccg2ptb -t $E/join00.a.out $E/join00.ptb \
  $D/ptb_rules.txt $D/ptb_unary.txt $D/ptb_leaves.txt > $E/join00.a.res

$SCRIPT/ccg2ptb -t $E/join00.g.out $E/join00.ptb \
  $D/ptb_rules.txt $D/ptb_unary.txt $D/ptb_leaves.txt > $E/join00.g.res

$SCRIPT/ccg2ptb -t $E/join23.a.out $E/join23.ptb \
  $D/ptb_rules.txt $D/ptb_unary.txt $D/ptb_leaves.txt > $E/join23.a.res

$SCRIPT/ccg2ptb -t $E/join23.g.out $E/join23.ptb \
  $D/ptb_rules.txt $D/ptb_unary.txt $D/ptb_leaves.txt > $E/join23.g.res

$SCRIPT/ccg2ptb -t $E/join00.ccg $E/join00.ptb ptb_rules.txt ptb_unary.txt ptb_leaves.txt \
  | tee $E/join00.bound \
  | $EVALB/evalb -p $EVALB/COLLINS.prm $E/join00.ptb /dev/stdin \
  | tee $E/join00.bound.evalb \
  | $SCRIPT/extract_ccg2ptb_subset "$COND" $E/$NAME $E/join00.*

$SCRIPT/ccg2ptb -t $E/join23.ccg $E/join23.ptb ptb_rules.txt ptb_unary.txt ptb_leaves.txt \
  | tee $E/join23.bound \
  | $EVALB/evalb -p $EVALB/COLLINS.prm $E/join23.ptb /dev/stdin \
  | tee $E/join23.bound.evalb \
  | $SCRIPT/extract_ccg2ptb_subset "$COND" $E/$NAME $E/join23.*

echo "UPPER BOUND WSJ 00 $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join00.ptb $E/$NAME/join00.bound \
  | tee $E/$NAME/join00.bound.evalb | grep 'Bracketing FMeasure'

echo "C&C WSJ 00 AUTO $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join00.ptb $E/$NAME/join00.a.res \
  | tee $E/$NAME/join00.a.evalb | grep 'Bracketing FMeasure'

echo "C&C WSJ 00 GOLD $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join00.ptb $E/$NAME/join00.g.res \
  | tee $E/$NAME/join00.g.evalb | grep 'Bracketing FMeasure'

echo "UPPER BOUND WSJ 23 $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join23.ptb $E/$NAME/join23.bound \
  | tee $E/$NAME/join23.bound.evalb | grep 'Bracketing FMeasure'

echo "BERKELEY WSJ 23 normal AUTO $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join23.ptb $E/$NAME/join23.berk.a \
  | tee $E/$NAME/join23.berk.a.evalb | grep 'Bracketing FMeasure'

echo "BERKELEY WSJ 23 accurate AUTO $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join23.ptb $E/$NAME/join23.bacc.a \
  | tee $E/$NAME/join23.bacc.a.evalb | grep 'Bracketing FMeasure'

echo "BERKELEY WSJ 23 normal GOLD $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join23.ptb $E/$NAME/join23.berk.g \
  | tee $E/$NAME/join23.berk.g.evalb | grep 'Bracketing FMeasure'

echo "BERKELEY WSJ 23 accurate GOLD $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join23.ptb $E/$NAME/join23.bacc.g \
  | tee $E/$NAME/join23.bacc.g.evalb | grep 'Bracketing FMeasure'

echo "C&C WSJ 23 AUTO $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join23.ptb $E/$NAME/join23.a.res \
  | tee $E/$NAME/join23.a.evalb | grep 'Bracketing FMeasure'

echo "C&C WSJ 23 GOLD $NAME $COND"

$EVALB/evalb -p $EVALB/COLLINS.prm $E/$NAME/join23.ptb $E/$NAME/join23.g.res \
  | tee $E/$NAME/join23.g.evalb | grep 'Bracketing FMeasure'

echo "SIGNIFICANCE TEST C&C vs BERKELEY accurate on AUTO $NAME $COND"

./compare.py $E/$NAME/join23.a.evalb $E/$NAME/join23.bacc.a.evalb e \
  | tee $E/$NAME/join23.a.sig | grep 'p-value'

echo "SIGNIFICANCE TEST C&C vs BERKELEY accurate on GOLD $NAME $COND"

./compare.py $E/$NAME/join23.g.evalb $E/$NAME/join23.bacc.g.evalb e \
  | tee $E/$NAME/join23.g.sig | grep 'p-value'
