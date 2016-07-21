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

source src/scripts/ccg/common

MODEL=$WORK/model_derivs_rev
LOG=$MODEL/log
FORESTS=/tmp/forests.rev

echo "creating model directory"

mkdir -p $MODEL
cat /dev/null > $LOG

echo "copying parser data from the source distribution" | tee -a $LOG

echo | tee -a $LOG

mkdir -p $MODEL/cats
cp $DATA/cats/* $MODEL/cats

echo "selecting feature types, applying cutoff and sorting" | tee -a $LOG

$SCRIPTS/count_features 1 '[abcdemnpqrstuvwFGHIJK]' $WORK/wsj02-21.feats > $MODEL/features

TRAIN_NSENTS=`grep '^$' $GEN/wsj02-21.markedup_deps | wc -l | sed 's/^ *//g'`
TRAIN_NSENTS=$[$TRAIN_NSENTS - 1]
NFEATURES=`grep -v '^$' $MODEL/features | grep -v '^# ' | wc -l | sed 's/^ *//g'`

echo | tee -a $LOG

echo "creating feature lexicon" | tee -a $LOG

$SCRIPTS/lexicon_features $MODEL/features > $MODEL/lexicon

echo | tee -a $LOG

echo "creating rules file" | tee -a $LOG

$SCRIPTS/extract_rules $GOLD/wsj02-21.pipe > $MODEL/rules

echo | tee -a $LOG

echo "creating config file" | tee -a $LOG

cat <<EOF > $MODEL/model.config
base $FORESTS
model $MODEL
iterations 10000
smoothing gaussian
alpha 0.6
norm_form true
EOF

echo "creating info file"

cat <<EOF > $MODEL/info
nsentences $TRAIN_NSENTS
nfeatures $NFEATURES
EOF
