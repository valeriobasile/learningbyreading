#!/bin/bash

PROGRAM=`basename $0`

if [ ! $# == 2 ]; then
  (
    echo "$PROGRAM: incorrect number of command line arguments"
    echo "usage: $PROGRAM <work> <questions>"
    echo "where: <work> is where the data should be stored"
    echo "       <questions> is is a file containing annotated questions"
  ) > /dev/stderr;
    exit 1;
fi

WORK=$1
QS=$2

BIN=bin
SCRIPTS=src/scripts/ccg

POS=$WORK/qpos
SUPER=$WORK/qsuper
GOLD=$WORK/gold

INPUT=$GOLD/wsj02-21.q10.stagged

echo "creating questions training file"

cat $GOLD/wsj02-21.stagged $QS $QS $QS $QS $QS $QS $QS $QS $QS $QS > $INPUT

echo "training questions POS tagger"

echo

mkdir -p $POS

$BIN/train_pos --model $POS --input $INPUT --ifmt '%w|%p|%s \n' --solver bfgs \
  --comment 'CCGbank sections 02-21' --niterations 500 --verbose | \
  tee $POS/log

echo "training questions supertagger"

echo

mkdir -p $SUPER

$BIN/train_super --model $SUPER --input $INPUT --solver bfgs \
  --comment 'CCGbank sections 02-21' --niterations 500 --verbose | \
  tee $SUPER/log
