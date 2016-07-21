#!/bin/bash

SET=$1
RTE=working/rte/${SET}
ALL=working/all.sym.raw
SORTED=working/all.sym.sorted
SYMIDF=working/symidf.pl

init()
{
    rm -f ${ALL}
    rm -f ${SYMIDF}
}

finit()
{
    rm -f ${ALL}
    rm -f ${SORTED}
}

compute()
{
    RTE=$1
    NDOC=`cat ${RTE}/*/kth.mod | grep model | grep d1 | wc -l | tr -s " "`
    echo "NDOC = ${NDOC}"
    for i in `ls ${RTE}/ | sort -n`; do
	if [ -f ${RTE}/$i/kth.mod ]; then
	    echo -n "$i "
	    cat ${RTE}/$i/kth.mod | grep "f(" | cut -d, -f2 >> ${ALL}
	fi
    done
    cat ${ALL} | sort | uniq -c | sort -n > ${SORTED}
    NSYM=`cat ${SORTED} | wc -l |tr -s " "`
    echo ""; echo "NSYM = ${NSYM}"
    for i in `seq 1 ${NSYM}`; do
        TERM=`cat ${SORTED} | head -$i | tail -1 | tr -s " "`
        FREQ=`echo ${TERM} | cut -d" " -f1` 
        SYMB=`echo ${TERM} | cut -d" " -f2` 
	IDF=`echo "l(${NDOC}/${FREQ})" | bc -l`
	if [ `echo ${IDF} | cut -c1` = "." ]; then
	    echo "idf(${SYMB}, 0${IDF}, ${FREQ})." >> ${SYMIDF}
	else
	    echo "idf(${SYMB}, ${IDF}, ${FREQ})." >> ${SYMIDF}
	fi
    done
    echo "ready"    
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
