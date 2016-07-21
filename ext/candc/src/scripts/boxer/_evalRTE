#!/bin/bash

SET=$1
RTE=working/rte/${SET}

check()
{
    if [ "${SET}" = "" ]; then
	echo "Usage: _evalRTE <RTESET>"
	echo "Example: _evalRTE rte3d"
	exit 1
    elif [ -d ${RTE} ]; then 
	echo "Checking directory: ${RTE}"
    else
	echo "Not a directory: ${RTE}"
	echo "Usage: _evalRTE <RTESET>"
	echo "Example: _evalRTE rte3d"
	exit 1
    fi
}

init()
{
    echo "------------------------------------"
    echo "Prediction (Method) -- Gold Standard"
    echo "------------------------------------"
    for i in `ls ${RTE}/ | sort -n`; do
	if [ -f ${RTE}/$i/prediction.txt ]; then 
	    echo "$i `cat ${RTE}/$i/prediction.txt` `cat ${RTE}/$i/gold.txt`"
	fi
    done > working/rte.results
    cat working/rte.results | cut -d" " -f2- | sort | uniq -c | sort -n | sort -k2
    echo "------------------------------------"    
    echo -n "Total number of instances:"
    cat working/rte.results | wc -l
    echo "------------------------------------"
}

count()
{
    cat working/rte.results | grep "$1" | wc -l
}


printacc()
{
   TOTAL=$(($2+$3))
   if [ $TOTAL -gt 0 ]; then
       echo -n "========> Accuracy: $1 (correct/incorrect) ="   
       echo -n " ($2/$3): 0."
       echo "$(((1000*$2)/($TOTAL))) (n=$TOTAL)"
   fi
}

accuracy()
{
    REPROOF=`count " proof) ent"`
    WEPROOF=`count " proof) inf"`
    printacc "proof" $REPROOF $WEPROOF

    RIPROOF=`count "inconsistency) inf"`
    WIPROOF=`count "inconsistency) ent"`
    printacc "inconsistency" $RIPROOF $WIPROOF
    RPROOF=$(($REPROOF+$RIPROOF))
    WPROOF=$(($WEPROOF+$WIPROOF))

    REMODEL=`count "entailed (model novelty) ent"`
    RIMODEL=`count "informative (model novelty) inf"`
    RMODEL=$(($REMODEL+$RIMODEL))
    WEMODEL=`count "entailed (model novelty) inf"`
    WIMODEL=`count "informative (model novelty) ent"`
    WMODEL=$(($WEMODEL+$WIMODEL))
    printacc "models" $RMODEL $WMODEL

    REWORDNET=`count "entailed (wordnet novelty) ent"`
    RIWORDNET=`count "informative (wordnet novelty) inf"`
    RWORDNET=$(($REWORDNET+$RIWORDNET))
    WIWORDNET=`count "informative (wordnet novelty) ent"`
    WEWORDNET=`count "entailed (wordnet novelty) inf"`
    WWORDNET=$(($WIWORDNET+$WEWORDNET))
    printacc "wordnet" $RWORDNET $WWORDNET

    REOVERLAP=`count "entailed (word overlap) ent"`
    RIOVERLAP=`count "informative (word overlap) inf"`
    ROVERLAP=$(($REOVERLAP+$RIOVERLAP))
    WIOVERLAP=`count "informative (word overlap) ent"`
    WEOVERLAP=`count "entailed (word overlap) inf"`
    WOVERLAP=$(($WIOVERLAP+$WEOVERLAP))
    printacc "overlap" $ROVERLAP $WOVERLAP

    RIGHT=$(($RMODEL+$RWORDNET+$RPROOF+$ROVERLAP))
    WRONG=$(($WMODEL+$WWORDNET+$WPROOF+$WOVERLAP))
    printacc "OVERALL" $RIGHT $WRONG

}

proofs()
{
    echo "Incorrect proofs:"
    cat working/rte.results | grep "proof) inf"
    echo "Correct proofs:"
    cat working/rte.results | grep "proof) ent"
}

finit()
{
    rm -f working/rte.results
}

check
init
accuracy
proofs
finit

exit 0
