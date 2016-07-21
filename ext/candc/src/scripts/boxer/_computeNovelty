#!/bin/bash

SET=$1
RTE=working/rte/${SET}
NOVELTY0=working/sym.novelty.0
NOVELTY1=working/sym.novelty.1
ARFF=working/novelty.arff

init()
{
    rm -f ${NOVELTY0}
    rm -f ${NOVELTY1}
    echo "@relation novelty" > ${ARFF}
    echo "@attribute model string" >> ${ARFF}
    echo "@attribute class {entailment,informative}" >> ${ARFF}
    echo "@data" >> ${ARFF}
}

finit()
{
    echo "ready"
}

compute()
{
    RTE=$1
    echo -n "Analysing "
    for i in `ls ${RTE}/ | sort -n`; do
	if [ -f ${RTE}/$i/novel.txt ]; then
	    if [ `cat ${RTE}/$i/novel.txt | wc -l` -ge 1 ]; then
		echo -n "$i "
		if [ `cat ${RTE}/$i/gold.txt | grep entail | wc -l` -gt 0 ]; then
		    cat ${RTE}/$i/novel.txt >> ${NOVELTY1}
		    echo -n "'" >> ${ARFF}
		    cat ${RTE}/$i/novel.txt | tr "\n" " " | sed "s/ $/',entailment/" >> ${ARFF}
		else
		    cat ${RTE}/$i/novel.txt >> ${NOVELTY0}
		    echo -n "'" >> ${ARFF}
		    cat ${RTE}/$i/novel.txt | tr "\n" " " | sed "s/ $/',informative/" >> ${ARFF}
		fi
	    fi
	fi
    done
    echo "ready"
    echo -n "** Novel (entailed)"; cat ${NOVELTY1} | wc -l
    cat ${NOVELTY1} | sort | uniq -c | sort -n -r | head -15
    echo -n "** Novel (informative)"; cat ${NOVELTY0} | wc -l
    cat ${NOVELTY0} | sort | uniq -c | sort -n -r | head -15
}

if [ "${SET}" = "" ]; then
    echo "Not a directory: ${RTE}"
    echo "Usage: _computeIDF <RTESET>"
    echo "Example: _computeIDF rte3"
elif [ -d ${RTE} ]; then 
    init
    compute ${RTE}
    finit
fi

exit 0
