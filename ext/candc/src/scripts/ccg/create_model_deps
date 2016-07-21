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

MODEL=$WORK/model_deps
LOG=$MODEL/log
FORESTS=/tmp/forests.deps

echo "creating model directory"

mkdir -p $MODEL

cat /dev/null > $LOG

echo "copying parser data from the source distribution" | tee -a $LOG

echo | tee -a $LOG

mkdir -p $MODEL/cats
cp $DATA/cats/* $MODEL/cats

echo "selecting feature types, applying cutoff and sorting" | tee -a $LOG

$SCRIPTS/count_features 1 "[abcdefghiLMNPQR]" $WORK/wsj02-21.feats > $MODEL/feature_counts

echo | tee -a $LOG

echo "creating feature_counts lexicon (used by count_rules)" | tee -a $LOG

$SCRIPTS/lexicon_features $MODEL/feature_counts > $MODEL/lexicon

echo | tee -a $LOG

echo "running count_rules using MPI" | tee -a $LOG

mkdir -p $MODEL/counts

mpiexec -wdir $BASE -n $NNODES $BIN/count_rules --nsents 39604 --super $SUPER \
  --data $WORK --counts $MODEL/counts/rules --parser $MODEL \
  --int-betas '0.1' --int-dict_cutoffs '20' \
  --parser-seen_rules false --parser-eisner_nf false \
  2>&1 | tee -a $LOG

echo | tee -a $LOG

echo "merging counts from cluster nodes" | tee -a $LOG

$SCRIPTS/merge_counts $MODEL/counts/rules*.out > $MODEL/rule_counts

echo | tee -a $LOG

echo "appending the rule counts to the end of features" | tee -a $LOG

(cat $MODEL/feature_counts; gawk '($1 >= 10)' $MODEL/rule_counts) > $MODEL/features

TRAIN_NSENTS=`grep '^$' $GEN/wsj02-21.markedup_deps | wc -l | sed 's/^ *//g'`
TRAIN_NSENTS=$[$TRAIN_NSENTS - 1]
NFEATURES=`grep -v '^$' $MODEL/features | grep -v '^# ' | wc -l | sed 's/^ *//g'`

echo | tee -a $LOG

echo "creating feature lexicon" | tee -a $LOG

$SCRIPTS/lexicon_features $MODEL/features > $MODEL/lexicon

echo | tee -a $LOG

echo "creating config file" | tee -a $LOG

cat <<EOF > $MODEL/model.config
base $FORESTS
model $MODEL
iterations 10000
smoothing gaussian
alpha 0.6
norm_form false
EOF

echo "creating info file" | tee -a $LOG

cat <<EOF > $MODEL/info
nsentences $TRAIN_NSENTS
nfeatures $NFEATURES
EOF
