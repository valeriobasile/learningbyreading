#!/bin/bash

source src/scripts/ccg/cl07_common

TABLE=$OUT/table5

mkdir -p $TABLE

cat /dev/null > $TABLE/log
cat /dev/null > $TABLE/results

echo "extracting statistics from models" | tee -a $TABLE/log

for i in model_deps model_derivs model_derivs_rev model_hybrid; do
  MODEL=$WORK/$i
  echo $i | tee -a $TABLE/results
  cat $MODEL/stats | tee -a $TABLE/results
done
