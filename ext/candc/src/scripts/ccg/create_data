#!/bin/bash
# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

if [ ! $# == 3 ]; then
  (
    echo "create_data: incorrect number of command line arguments"
    echo "usage: create_data <CCGbank> <nnodes> <work>"
    echo "where: <CCGbank> is the directory containing the LDC CCGbank release"
    echo "       <nnodes> is the number of cluster nodes to be used for training"
    echo "       <work> is where the data should be stored"
  ) > /dev/stderr;
    exit 1;
fi
if [ ! -r bin/generate ]; then
  echo 'bin/generate is missing ($ make bin/generate)' >&2
  exit 1
fi

CCGBANK=$1
NNODES=$2
WORK=$3

BIN=bin
SCRIPTS=src/scripts/ccg
DATA=src/data/ccg

GOLD=$WORK/gold
GEN=$WORK/generated
LOG=$WORK/log
MARKEDUP=$DATA/cats/markedup

mkdir -p $WORK

cat <<EOF > $WORK/README
This directory was created automatically using $SCRIPTS/create_data.
The data files were generated directly from the pipe files from CCGBank
(in the $CCGBANK directory) and the manually created data
that is part of the parser itself (in the $DATA directory).
EOF

mkdir -p $GOLD
mkdir -p $GEN
cat /dev/null > $LOG

echo 'converting AUTO files to old .pipe format' | tee -a $LOG

$SCRIPTS/convert_auto $CCGBANK/data/AUTO/00/*.auto | $SCRIPTS/convert_brackets > $GOLD/wsj00.pipe

# error in sentence 7712 (CCGbank.final/AUTO/05/wsj_0595.auto)
# fixed category ((S[b]\NP)/NP)/ -> (S[b]\NP)/NP
$SCRIPTS/convert_auto $CCGBANK/data/AUTO/0[2-9]/*.auto $CCGBANK/data/AUTO/1*/*.auto $CCGBANK/data/AUTO/2[01]/*.auto | \
 sed 's|((S\[b\]\\NP)/NP)/ |(S[b]\\NP)/NP |g' | $SCRIPTS/convert_brackets > $GOLD/wsj02-21.pipe

$SCRIPTS/convert_auto $CCGBANK/data/AUTO/23/*.auto | $SCRIPTS/convert_brackets > $GOLD/wsj23.pipe

echo 'creating CCGbank PARG files (grouped by section)' | tee -a $LOG

cat $CCGBANK/data/PARG/00/*.parg | $SCRIPTS/convert_brackets > $GOLD/wsj00.parg
cat $CCGBANK/data/PARG/0[2-9]/*.parg $CCGBANK/data/PARG/1*/*.parg $CCGBANK/data/PARG/2[01]/*.parg | \
  $SCRIPTS/convert_brackets > $GOLD/wsj02-21.parg
cat $CCGBANK/data/PARG/23/*.parg | $SCRIPTS/convert_brackets > $GOLD/wsj23.parg

echo 'extracting CCGbank POS/super-tagged text' | tee -a $LOG

$SCRIPTS/extract_sequences -s $GOLD/wsj02-21.pipe > $GOLD/wsj02-21.stagged
$SCRIPTS/extract_sequences -s $GOLD/wsj00.pipe > $GOLD/wsj00.stagged
$SCRIPTS/extract_sequences -s $GOLD/wsj23.pipe > $GOLD/wsj23.stagged

$SCRIPTS/extract_sequences -t $GOLD/wsj02-21.pipe > $GOLD/wsj02-21.pos
$SCRIPTS/extract_sequences -t $GOLD/wsj00.pipe > $GOLD/wsj00.pos
$SCRIPTS/extract_sequences -t $GOLD/wsj23.pipe > $GOLD/wsj23.pos

$SCRIPTS/extract_sequences -w $GOLD/wsj02-21.pipe > $GOLD/wsj02-21.raw
$SCRIPTS/extract_sequences -w $GOLD/wsj00.pipe > $GOLD/wsj00.raw
$SCRIPTS/extract_sequences -w $GOLD/wsj23.pipe > $GOLD/wsj23.raw

echo 'extracting CCGbank category lists' | tee -a $LOG

$SCRIPTS/extract_cats $GOLD/wsj02-21.stagged > $GOLD/wsj02-21.cats
$SCRIPTS/extract_cats $GOLD/wsj00.stagged > $GOLD/wsj00.cats

echo 'converting PARG files into our dependency format' | tee -a $LOG

$SCRIPTS/parg2ccgbank_deps $GOLD/wsj00.parg > $GOLD/wsj00.ccgbank_deps
$SCRIPTS/parg2ccgbank_deps $GOLD/wsj02-21.parg > $GOLD/wsj02-21.ccgbank_deps
$SCRIPTS/parg2ccgbank_deps $GOLD/wsj23.parg > $GOLD/wsj23.ccgbank_deps

(
echo
echo 'generating training dependencies and features from wsj02-21.pipe';
echo 'bin/generate -t produces markedup slot dependencies with the positions on stdout';
echo '                it also prints the features file on stderr'
) | tee -a $LOG

bin/generate -t $DATA/cats $MARKEDUP $GOLD/wsj02-21.pipe > $GEN/wsj02-21.markedup_deps 2> $WORK/wsj02-21.feats
mv errors.log $GEN/wsj02-21.gen.log

TRAIN_NSENTS=`grep '^$' $GEN/wsj02-21.markedup_deps | wc -l | sed 's/^ *//g'`
TRAIN_NSENTS=$[$TRAIN_NSENTS - 1]
TRAIN_NFAILS=`gawk 'BEGIN { end = 0; } /^$/ { if(end) print "failed"; end = 1; next; } { end = 0; }' < $GEN/wsj02-21.markedup_deps | wc -l | sed 's/^ *//g'`

(
echo 'number of training sentences' $TRAIN_NSENTS
echo 'number of training parse failures' $TRAIN_NFAILS

echo

echo 'generating development dependencies from wsj00.pipe'
echo 'bin/generate -e produces markedup slot dependencies,'
echo '  and with the words rather than positions'
echo "these go into $GEN/wsj00.markedup_deps"
) | tee -a $LOG

bin/generate -e $DATA/cats $MARKEDUP $GOLD/wsj00.pipe > $GEN/wsj00.markedup_deps
mv errors.log $GEN/wsj00.gen.log

DEV_NSENTS=`grep '^$' $GEN/wsj00.markedup_deps | wc -l | sed 's/^ *//g'`
DEV_NSENTS=$[$DEV_NSENTS - 1]
DEV_NFAILS=`gawk 'BEGIN { end = 0; } /^$/ { if(end) print "failed"; end = 1; next; } { end = 0; }' < $GEN/wsj00.markedup_deps | wc -l | sed 's/^ *//g'`

(
echo 'number of development sentences' $DEV_NSENTS
echo 'number of development parse failures' $DEV_NFAILS

echo 'generating development dependencies from wsj00.pipe'
echo 'bin/generate -j produces CCGbank slot dependencies,'
echo '  and with the words rather than positions'
echo 'these are only used for parser development purposes'
echo "these go into $GEN/wsj00.ordered_deps"
) | tee -a $LOG

bin/generate -j $DATA/cats $MARKEDUP $GOLD/wsj00.pipe > $GEN/wsj00.ordered_deps

(
echo

echo 'generating testing dependencies from wsj23.pipe'
echo 'bin/generate -e produces markedup slot dependencies,'
echo '  and with the words rather than positions'
echo "these go into $GEN/wsj23.markedup_deps"
) | tee -a $LOG

bin/generate -e $DATA/cats $MARKEDUP $GOLD/wsj23.pipe > $GEN/wsj23.markedup_deps
mv errors.log $GEN/wsj23.gen.log

TEST_NSENTS=`grep '^$' $GEN/wsj23.markedup_deps | wc -l | sed 's/^ *//g'`
TEST_NSENTS=$[$TEST_NSENTS - 1]
TEST_NFAILS=`gawk 'BEGIN { end = 0; } /^$/ { if(end) print "failed"; end = 1; next; } { end = 0; }' < $GEN/wsj23.markedup_deps | wc -l | sed 's/^ *//g'`

(
echo 'number of test sentences' $TEST_NSENTS
echo 'number of test parse failures' $TEST_NFAILS

echo 'generating testing dependencies from wsj23.pipe'
echo 'bin/generate -j produces CCGbank slot dependencies,'
echo '  and with the words rather than positions'
echo "these go into $GEN/wsj23.ordered_deps"
) | tee -a $LOG

bin/generate -j $DATA/cats $MARKEDUP $GOLD/wsj23.pipe > $GEN/wsj23.ordered_deps

(
echo

echo "splitting training features into $NNODES files in $WORK/feats" | tee -a $LOG
) | tee -a $LOG

mkdir -p $WORK/feats

$SCRIPTS/distribute $TRAIN_NSENTS $NNODES '^$' $WORK/feats $WORK/wsj02-21.feats

(
echo

echo "splitting training dependencies into $NNODES files in $WORK/deps"
) | tee -a $LOG

mkdir -p $WORK/deps

$SCRIPTS/distribute $TRAIN_NSENTS $NNODES '^$' $WORK/deps $GEN/wsj02-21.markedup_deps

(
echo

echo "splitting training stagged sentences into $NNODES files in $WORK/stag"
) | tee -a $LOG

mkdir -p $WORK/stag

$SCRIPTS/distribute $TRAIN_NSENTS $NNODES '.' $WORK/stag $GOLD/wsj02-21.stagged
