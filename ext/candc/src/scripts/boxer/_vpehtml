#!/bin/bash

VPE=src/data/vpe/wsj
WSJ=working/wsj
PTB=working/PTB2.0/raw/wsj

BEGTAGANT="<font color="green"><b>"
ENDTAGANT="</b></font>"
BEGTAGVPE="<font color="blue"><b>"
ENDTAGVPE="</b></font>"

printline()
{
    CHARS=`echo $LINE | wc -m`
    for j in `seq 1 $CHARS`; do
	if [ "$BEG" == "$j" ]; then
	    echo -n "$BEGTAGANT" >> $OUTFILE
	fi
	if [ "$BVP" == "$j" ]; then
	    echo -n "$BEGTAGVPE" >> $OUTFILE
	fi
	KAR=`echo $LINE | cut -c$j`
	echo -n "$KAR" >> $OUTFILE
	if [ "$END" == "$j" ]; then
	    echo -n "$ENDTAGANT" >> $OUTFILE
	fi
	if [ "$EVP" == "$j" ]; then
	    echo -n "$ENDTAGVPE" >> $OUTFILE
	fi
    done
}


makeattributes()
{
for ATTRIBUTE in 8 9 10; do

	case "$ATTRIBUTE" in
	    8) echo "<li>Sorted on auxilary form of VPE</li>" >> $INDFILE ;;
	    9) echo "<li>Sorted on syntactic type of antecedent</li>" >> $INDFILE ;;
	    10) echo "<li>Sorted on source-target pattern</li>" >> $INDFILE ;;
	    11) echo "<li>Sorted on discourse relation</li>" >> $INDFILE ;;
	    *) echo "<li>$ATTRIBUTE</li>" >> $INDFILE ;;
	esac
	echo "<p>" >> $INDFILE
	TYPES=`cat $VPE/?? | cut -d" " -f$ATTRIBUTE | sort | uniq -c | sort -n -r | tr -s " " | cut -d" " -f3`
	for TYPE in $TYPES; do
	    
	    NFILES=`cat $VPE/?? | cut -d" " -f1-7,$ATTRIBUTE | grep " ${TYPE}$" | cut -d" " -f1-7 | wc -l | tr -s " " | sed 's/^ //'`
	    
	    if [ $ATTRIBUTE == "10" ]; then
		DESCRIPTION=`cat src/data/vpe/type.pl | grep "^ct($TYPE" | cut -d"'" -f2 \
		    | sed 's/\\$\\\mid\\$/|/g' \
		    | sed 's/\\$_{src}\\$/src/' \
		    | sed 's/\\$_{tgt}\\$/tgt/'`
	    elif [ $ATTRIBUTE == "11" ]; then
		DESCRIPTION=`cat src/data/vpe/type.pl | grep "^dr($TYPE" | cut -d"'" -f2`
	    else
		DESCRIPTION=$TYPE
	    fi
	    
	    OUTFILE=$WSJ/wsj.att${ATTRIBUTE}${TYPE}.html
	    echo $OUTFILE
	    if [ "${TYPE}" == "${DESCRIPTION}" ]; then
		echo "<a href="wsj.att${ATTRIBUTE}${TYPE}.html">$DESCRIPTION</a> " >> $INDFILE
	    else
		echo "<p><a href="wsj.att${ATTRIBUTE}${TYPE}.html">$TYPE</a> $DESCRIPTION</p>" >> $INDFILE
	    fi
	    echo "<html>" > $OUTFILE
	    echo "<head>" >> $OUTFILE
	    echo "<title>VPE: attribute $ATTRIBUTE ($DESCRIPTION)</title>" >> $OUTFILE
	    echo "</head>" >> $OUTFILE
	    echo "<body>" >> $OUTFILE
	    
	    echo "<h2>$DESCRIPTION (${NFILES} cases)</h2>" >> $OUTFILE
	    for N in `seq 1 $NFILES`; do
		INFO=`cat $VPE/?? | cut -d" " -f1-7,$ATTRIBUTE | grep " ${TYPE}$" | cut -d" " -f1-7 | head -$N | tail -1`
		SECTION=`echo $INFO | cut -c5,6`
		FILE=`echo $INFO | cut -d" " -f1`
		REGELVPE=`echo $INFO | cut -d" " -f2`
		REGELANT=`echo $INFO | cut -d" " -f5`
		SEARCH=`echo $INFO | cut -d" " -f1-7`
		ANT=`cat $VPE/?? | grep "^$SEARCH" | cut -d" " -f9`
		echo "<p><b>${FILE} (line ${REGELVPE}, ant ${ANT}):</b><br>" >> $OUTFILE
		if [ $REGELVPE == $REGELANT ]; then
		    BVP=`echo $INFO | cut -d" " -f3`
		    EVP=`echo $INFO | cut -d" " -f4`
		    BEG=`echo $INFO | cut -d" " -f6`
		    END=`echo $INFO | cut -d" " -f7`
		    LINE=`cat $PTB/$SECTION/$FILE | head -$REGELVPE | tail -1`
		    printline
		else
		    BVP="0"
		    EVP="0"
		    BEG=`echo $INFO | cut -d" " -f6`
		    END=`echo $INFO | cut -d" " -f7`
		    LINE=`cat $PTB/$SECTION/$FILE | head -$REGELANT | tail -1`
		    printline
		    echo "<br>" >> $OUTFILE
		    BVP=`echo $INFO | cut -d" " -f3`
		    EVP=`echo $INFO | cut -d" " -f4`
		    BEG="0"
		    END="0"
		    LINE=`cat $PTB/$SECTION/$FILE | head -$REGELVPE | tail -1`
		    printline
		fi
		echo "</p>" >> $OUTFILE
	    done
	    echo "</body>" >> $OUTFILE
	    echo "</html>" >> $OUTFILE
	done
	echo "</p>" >> $INDFILE
    done
}

makesections()
{
    echo "<li>Sorted on WSJ section</li>" >> $INDFILE
    echo "<p>" >> $INDFILE
    for SECTION in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24; do
	
	NFILES=`cat $VPE/$SECTION | wc -l | tr -s " " | sed 's/^ //'`
	OUTFILE=$WSJ/wsj.section${SECTION}.html
	echo $OUTFILE
	echo "<a href="wsj.section${SECTION}.html">$SECTION</a> " >> $INDFILE
	echo "<html>" > $OUTFILE
	echo "<head>" >> $OUTFILE
	echo "<title>VPE: WSJ section ($SECTION)</title>" >> $OUTFILE
	echo "</head>" >> $OUTFILE
	echo "<body>" >> $OUTFILE

	echo "<h1>WSJ section $SECTION (${NFILES} cases)</h1>" >> $OUTFILE
	for N in `seq 1 $NFILES`; do
	    INFO=`cat $VPE/$SECTION | cut -d" " -f1-7 | head -$N | tail -1`
	    SECTION=`echo $INFO | cut -c5,6`
	    FILE=`echo $INFO | cut -d" " -f1`
	    REGELVPE=`echo $INFO | cut -d" " -f2`
	    REGELANT=`echo $INFO | cut -d" " -f5`
            SEARCH=`echo $INFO | cut -d" " -f1-7`
            ANT=`cat $VPE/$SECTION | grep "^$SEARCH" | cut -d" " -f9`
	    echo "<p><b>${FILE} (line ${REGELVPE}, ant ${ANT}):</b><br>" >> $OUTFILE
	    if [ $REGELVPE == $REGELANT ]; then
		BVP=`echo $INFO | cut -d" " -f3`
		EVP=`echo $INFO | cut -d" " -f4`
		BEG=`echo $INFO | cut -d" " -f6`
		END=`echo $INFO | cut -d" " -f7`
		LINE=`cat $PTB/$SECTION/$FILE | head -$REGELVPE | tail -1`
		printline
	    else
		BVP="0"
		EVP="0"
		BEG=`echo $INFO | cut -d" " -f6`
		END=`echo $INFO | cut -d" " -f7`
		LINE=`cat $PTB/$SECTION/$FILE | head -$REGELANT | tail -1`
		printline
		echo "<br>" >> $OUTFILE
		BVP=`echo $INFO | cut -d" " -f3`
		EVP=`echo $INFO | cut -d" " -f4`
		BEG="0"
		END="0"
		LINE=`cat $PTB/$SECTION/$FILE | head -$REGELVPE | tail -1`
		printline
	    fi
	    echo "</p>" >> $OUTFILE
	done
	echo "</body>" >> $OUTFILE
	echo "</html>" >> $OUTFILE
    done
    echo "</p>" >> $INDFILE
}

mkdir -p $WSJ
INDFILE=$WSJ/index.html
echo "<html>" > $INDFILE
echo "<head>" >> $INDFILE
echo "<title>An annotated corpus for the analysis of VP ellipsis</title>" >> $INDFILE
echo "</head>" >> $INDFILE
echo "<body>" >> $INDFILE
echo "<h1>An annotated corpus for the analysis of VP ellipsis</h1>" >> $INDFILE
echo '<h2>By <a href="http://www.let.rug.nl/bos/">Johan Bos</a> and <a href="http://www.ai.rug.nl/~spenader/">Jennifer Spenader</a></h2>' >> $INDFILE
echo '<p><b><a href="http://www.let.rug.nl/bos/vpe/abstract.html">Abstract</a>' >> $INDFILE
echo '      <a href="http://www.let.rug.nl/bos/vpe/corpus/">Examples</a>' >>$INDFILE
echo '      <a href="http://www.let.rug.nl/bos/vpe/article.html">Article</a>' >>$INDFILE
echo '      <a href="http://www.let.rug.nl/bos/vpe/annotations.html">Annotations</a></b></p>' >>$INDFILE
echo "<ul>" >> $INDFILE
makesections
makeattributes
echo "</ul>" >> $INDFILE
echo "</body>" >> $INDFILE
echo "</html>" >> $INDFILE

exit 0
