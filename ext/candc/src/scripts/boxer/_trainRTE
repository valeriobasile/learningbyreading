#!/bin/bash

SET=$1
RTE=working/rte/${SET}
ARF0=working/TaskEverything.arff
ARF1=working/Everything.arff
ARF2=working/WordNet.arff
ARF3=working/Overlap.arff
ARF4=working/ModNovelty.arff
ARF5=working/DomNovelty.arff
ARF6=working/RelNovelty.arff
ARF7=working/DomRelNovelty.arff

train()
{
    RTE=$1
    for i in `ls ${RTE}/ | sort -n`; do
	echo -n "$i "
	if [ -f ${RTE}/$i/task.txt ]; then
	    TASK=`cat ${RTE}/$i/task.txt`
	else
	    TASK="UNKNOWN"
	fi
	if [ -f ${RTE}/$i/modsizedif.txt ]; then
	    if [ `cat ${RTE}/$i/prediction.txt | grep -v 'error' | wc -l` -ge 1 ]; then

		WORDNET=`cat ${RTE}/$i/modsizedif.txt | grep 'wordnet novelty' | cut -d' ' -f1 | sed 's/.$//'`

		echo -n "'${TASK}'," >> $ARF0
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'prover' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF0
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'domain novelty' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF0
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'relation novelty' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF0
		echo -n "${WORDNET}," >> $ARF0
		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'word overlap' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF0
		echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF0

		if [ `echo ${WORDNET} | cut -c1` = "-" ]; then
		    echo -n "skipping ${WORDNET} wordnet "
		else
		    echo -n "${WORDNET}," >> $ARF2
		    echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF2
		fi

		echo -n "`cat ${RTE}/$i/modsizedif.txt | grep 'word overlap' | cut -d' ' -f1 | sed 's/.$//'`," >> $ARF3
		echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF3

		if [ `cat ${RTE}/$i/modsizedif.txt | grep 'model novelty' | wc -l` -ge 1 ]; then
		    MODNOVELTY=`cat ${RTE}/$i/modsizedif.txt | grep 'model novelty' | cut -d' ' -f1 | sed 's/.$//'`
		    DOMNOVELTY=`cat ${RTE}/$i/modsizedif.txt | grep 'domain novelty' | cut -d' ' -f1 | sed 's/.$//'`
		    RELNOVELTY=`cat ${RTE}/$i/modsizedif.txt | grep 'relation novelty' | cut -d' ' -f1 | sed 's/.$//'`
		    if [ `echo ${DOMNOVELTY} | cut -c1` = "-" ]; then
			echo -n "skipping ${DOMNOVELTY} novelty "
		    else
			echo -n "${MODNOVELTY}," >> $ARF4
			echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF4
			echo -n "${DOMNOVELTY}," >> $ARF5
			echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF5
			echo -n "${RELNOVELTY}," >> $ARF6
			echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF6
			echo -n "${DOMNOVELTY}," >> $ARF7
			echo -n "${RELNOVELTY}," >> $ARF7
			echo    "`cat ${RTE}/$i/gold.txt`" >> $ARF7
		    fi
		fi
	    fi
	fi
    done
    echo "ready"
    
}

init()
{
    echo "Checking directory: ${RTE}"
    
    echo "@relation ${SET}TaskEverything" > $ARF0
    echo "@attribute task {'IE', 'IR', 'QA', 'SUM'}" >> $ARF0
    echo "@attribute prover {proof, contradiction, unknown}" >> $ARF0
    echo "@attribute dom_novelty real" >> $ARF0
    echo "@attribute rel_novelty real" >> $ARF0
    echo "@attribute mwn_novelty real" >> $ARF0
    echo "@attribute word_overlap real" >> $ARF0
    echo "@attribute gold {informative, entailment}" >> $ARF0
    echo "@data" >> $ARF0

    echo "@relation ${SET}WordNet" > $ARF2
    echo "@attribute novelty real" >> $ARF2
    echo "@attribute gold {informative, entailment}" >> $ARF2
    echo "@data" >> $ARF2

    echo "@relation ${SET}WordOverlap" > $ARF3
    echo "@attribute overlap real" >> $ARF3
    echo "@attribute gold {informative, entailment}" >> $ARF3
    echo "@data" >> $ARF3

    echo "@relation ${SET}ModNovelty" > $ARF4
    echo "@attribute novelty real" >> $ARF4
    echo "@attribute gold {informative, entailment}" >> $ARF4
    echo "@data" >> $ARF4

    echo "@relation ${SET}DomNovelty" > $ARF5
    echo "@attribute novelty real" >> $ARF5
    echo "@attribute gold {informative, entailment}" >> $ARF5
    echo "@data" >> $ARF5

    echo "@relation ${SET}RelNovelty" > $ARF6
    echo "@attribute novelty real" >> $ARF6
    echo "@attribute gold {informative, entailment}" >> $ARF6
    echo "@data" >> $ARF6

    echo "@relation ${SET}DomRelNovelty" > $ARF7
    echo "@attribute dom_novelty real" >> $ARF7
    echo "@attribute rel_novelty real" >> $ARF7
    echo "@attribute gold {informative, entailment}" >> $ARF7
    echo "@data" >> $ARF7
}

classify()
{
    echo "-------------------------------"
    echo "Classifier for $1"
    echo "-------------------------------"
    CLASSPATH=/net/aps/32/weka-3-6-3/weka.jar
    java -classpath ${CLASSPATH} weka.classifiers.trees.J48 -t $1
}

if [ "${SET}" = "" ]; then
    echo "Not a directory: ${RTE}"
    echo "Usage: _trainRTE <RTESET>"
    echo "Example: _trainRTE dev3"
elif [ -d ${RTE} ]; then 
    init
    train ${RTE}
    cat ${ARF0} | grep "^@" | grep -v "task" > ${ARF1}
    cat ${ARF0} | grep "^'" | cut -d"," -f2- >> ${ARF1}

#    classify ${ARF2simple}
#    classify ${ARF1}
#    classify ${ARF2}
#    classify ${ARF3simple}
#    classify ${ARF3}
#    classify ${ARF4}
fi

exit 0
