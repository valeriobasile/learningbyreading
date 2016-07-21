#!/bin/bash

PROGRAM=`basename $0`

if [ ! $# == 1 ]; then
  (
    echo "$PROGRAM: incorrect number of command line arguments"
    echo "usage: $PROGRAM <work>"
    echo "where: <work> is where the data should be stored"
  ) > /dev/stderr;
    exit 1;
fi

WORK=$1

BIN=bin
SCRIPTS=src/scripts/ccg

POS=$WORK/pos
SUPER=$WORK/super
GOLD=$WORK/gold

echo "training POS tagger"

echo

mkdir -p $POS

$BIN/train_pos --model $POS --input $GOLD/wsj02-21.pos --solver bfgs \
  --comment 'CCGbank sections 02-21' --niterations 500 --verbose | \
  tee $POS/log

echo "training supertagger"

echo

mkdir -p $SUPER

$BIN/train_super --model $SUPER --input $GOLD/wsj02-21.stagged --solver bfgs \
  --comment 'CCGbank sections 02-21' --niterations 500 --verbose | \
  tee $SUPER/log
