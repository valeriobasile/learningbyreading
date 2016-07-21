#!/bin/bash

VPE=src/data/vpe/wsj
WSJ=working/wsj
PTB=working/PTB2.0/raw/wsj

BEGTAGANT="<font color="red"><b>"
ENDTAGANT="</b></font>"
BEGTAGVPE="<font color="blue"><b>"
ENDTAGVPE="</b></font>"

for SECTION in `ls $PTB`; do
    if test -d $PTB/$SECTION; then
        OUTFILE=$WSJ/wsj.$SECTION.html
        echo "out: $OUTFILE"
	echo "<html>" > $OUTFILE
	echo "<head>" >> $OUTFILE
	echo -n "<title>" >> $OUTFILE
	echo -n $FILE >> $OUTFILE
	echo "</title>" >> $OUTFILE
	echo "</head>" >> $OUTFILE
	echo "<body>" >> $OUTFILE
	echo '<table border ="1">' >> $OUTFILE

	for FILE in `ls $PTB/$SECTION`; do
	    LEN=`cat $PTB/$SECTION/$FILE | wc -l`
	    
	    for i in `seq 1 $LEN`; do
		LINE=`cat $PTB/$SECTION/$FILE | head -$i | tail -1`
		if test -f $VPE/$SECTION ; then
		    if [ `cat $VPE/$SECTION | cut -d" " -f1,5 | grep "$FILE $i$" | wc -l` -gt 0 -o \
			 `cat $VPE/$SECTION | cut -d" " -f1,2 | grep "$FILE $i$" | wc -l` -gt 0 ]; then
			echo -n "<tr><td>$FILE</td><td>$i</td><td>" >> $OUTFILE

			CHARS=`echo $LINE | wc -m`
			BEG=`cat $VPE/$SECTION | cut -d" " -f1,5,6 | grep "$FILE $i " | cut -d" " -f3`
			END=`cat $VPE/$SECTION | cut -d" " -f1,5,7 | grep "$FILE $i " | cut -d" " -f3`
			BVP=`cat $VPE/$SECTION | cut -d" " -f1,2,3 | grep "$FILE $i " | cut -d" " -f3`
			EVP=`cat $VPE/$SECTION | cut -d" " -f1,2,4 | grep "$FILE $i " | cut -d" " -f3`
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

			echo "</td></tr>" >> $OUTFILE
		    fi
		fi
	    done

	done
	echo "</table>" >> $OUTFILE
	echo "</body>" >> $OUTFILE
	echo "</html>" >> $OUTFILE
    fi
done

exit 0
