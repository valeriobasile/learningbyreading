#!/bin/bash

VPE=src/data/vpe/wsj
PTB=working/PTB2.0/raw/wsj
WSJ=working/wsj

mkdir -p $WSJ

BEGTAGANT="<ANT"
ENDTAGANT="</ANT"
BEGTAGVPE="<VPE"


tokenise()
{
    cat $OUTFILE0 | grep "." | grep -v "^\.START" >> $OUTFILE3
    cat $OUTFILE0 | grep "." | grep -v "^\.START" \
         | sed "s/S.p.$/S.p./" \
         | bin/tokkie --stdin --quotes delete | sed 's/ <\//<\//' | cut -d" " -f1-249 >> $OUTFILE1
}

for SECTION in `ls $PTB`; do
    if test -d $PTB/$SECTION; then
	OUTFILE3=$WSJ/wsj.sentence.$SECTION.txt
	OUTFILE2=$WSJ/wsj.sentence.$SECTION.tok
	OUTFILE1=$WSJ/wsj.discourse.$SECTION.tok
        OUTFILE0=working/tmp.wsj.txt
	echo "out: $OUTFILE1"
	rm -fr $OUTFILE1
	touch $OUTFILE1
	rm -fr $OUTFILE3
	touch $OUTFILE3
	for FILE in `ls $PTB/$SECTION`; do
	    rm -fr $OUTFILE0
	    touch $OUTFILE0
	    LEN=`cat $PTB/$SECTION/$FILE | wc -l`
	    for i in `seq 1 $LEN`; do
		LINE=`cat $PTB/$SECTION/$FILE | head -$i | tail -1`
		if test -f $VPE/$SECTION ; then
		    if [ `cat $VPE/$SECTION | cut -d" " -f1,5 | grep "$FILE $i$" | wc -l` -gt 0 -o \
			`cat $VPE/$SECTION | cut -d" " -f1,2 | grep "$FILE $i$" | wc -l` -gt 0 ]; then
			CHARS=`echo $LINE | wc -m`
			NBEG=`cat $VPE/$SECTION | cut -d" " -f1,5,6 | grep "$FILE $i " | wc -l`
			NEND=`cat $VPE/$SECTION | cut -d" " -f1,5,7 | grep "$FILE $i " | wc -l`
			NBVP=`cat $VPE/$SECTION | cut -d" " -f1,2,3 | grep "$FILE $i " | wc -l`
			for j in `seq 1 $CHARS`; do
			    if [ $NBEG -gt 0 ]; then
				for k in `seq 1 $NBEG`; do
				    BEG=`cat $VPE/$SECTION | cut -d" " -f1,5,6 | grep "$FILE $i " | head -$k | tail -1 | cut -d" " -f3`
				    if [ "$BEG" == "$j" ]; then
					ID=`cat -n $VPE/$SECTION | tr "\t" " " | tr -s " " \
					    | cut -d" " -f2,3,7 | grep "$FILE $i$" | head -$k | tail -1 | cut -d" " -f1`
					echo -n "$BEGTAGANT-$ID>" >> $OUTFILE0
				    fi
				done
			    fi
			    if [ $NBVP -gt 0 ]; then
				for k in `seq 1 $NBVP`; do
				    BVP=`cat $VPE/$SECTION | cut -d" " -f1,2,3 | grep "$FILE $i " | head -$k | tail -1 | cut -d" " -f3`
				    if [ "$BVP" == "$j" ]; then
					ID=`cat -n $VPE/$SECTION | tr "\t" " " | tr -s " " \
					    | cut -d" " -f2,3,4 | grep "$FILE $i$" | head -$k | tail -1 | cut -d" " -f1`
					VPETYPE=`cat $VPE/$SECTION | tr "\t" " " | tr -s " " \
					    | cut -d" " -f1,2,8 | grep "$FILE $i " | head -$k | tail -1 | cut -d" " -f3`
					VPEANTE=`cat $VPE/$SECTION | tr "\t" " " | tr -s " " \
					    | cut -d" " -f1,2,9 | grep "$FILE $i " | head -$k | tail -1 | cut -d" " -f3`
					echo -n "$BEGTAGVPE-$ID-$VPETYPE-$VPEANTE>" >> $OUTFILE0
				    fi
				done
			    fi
			    KAR=`echo $LINE | cut -c$j`
			    echo -n "$KAR" >> $OUTFILE0
			    if [ $NEND -gt 0 ]; then
				for k in `seq 1 $NEND`; do
				    END=`cat $VPE/$SECTION | cut -d" " -f1,5,7 | grep "$FILE $i " | head -$k | tail -1 | cut -d" " -f3`
				    if [ "$END" == "$j" ]; then
					ID=`cat -n $VPE/$SECTION | tr "\t" " " | tr -s " " \
					    | cut -d" " -f2,3,7 | grep "$FILE $i$" | head -$k | tail -1 | cut -d" " -f1`
					echo -n "$ENDTAGANT-$ID>" >> $OUTFILE0
				    fi
				done
			    fi
			done
                        echo "" >> $OUTFILE0
		    else
			echo "$LINE" >> $OUTFILE0
		    fi
		else
		    echo "$LINE" >> $OUTFILE0
		fi
	    done
	    META=`echo $FILE`
	    echo "<META>$META" >> $OUTFILE1
	    tokenise
	done

	ANNFILE=$WSJ/vpe$SECTION.pl
	rm -f $ANNFILE
        touch $ANNFILE
        cat $OUTFILE1 | grep -v "<META>" > $OUTFILE0
	LEN=`cat $OUTFILE0 | wc -l`	
	for i in `seq 1 $LEN`; do
	    LINE=`cat $OUTFILE0 | head -$i | tail -1`
	    if [ `echo $LINE | grep "<" | wc -l` -gt 0 ]; then
		TOKENS=`echo $LINE | tr -s " " | tr " " "\n" | wc -l`
		for j in `seq 1 $TOKENS`; do
		    THETOK=`echo $LINE | tr -s " " | cut -d" " -f$j`

		    # here $TOK can contain more than one XML tag
		    NTAGS=`echo -n $THETOK | tr ">" "\n" | wc -l`
		    if [ $NTAGS -gt 0 ]; then
			for t in `seq 1 $NTAGS`; do

			    TOK=`echo $THETOK | cut -d">" -f$t`
			    if [ `echo $TOK | grep '[a-z]<VPE-' | wc -l` -gt 0 ]; then
				VPID=`echo $TOK | cut -d"-" -f2`
				VPETYPE=`echo $TOK | cut -d"-" -f3`
				VPEANTE=`echo $TOK | cut -d"-" -f4 | cut -d">" -f1`
				newj=$((j+1));
				echo "vpe($VPID, wsj$SECTION, $i, $newj). %%% $TOK" >> $ANNFILE
				echo "vpe_type($VPID, wsj$SECTION, $VPETYPE, $VPEANTE)." >> $ANNFILE
			    elif [ `echo $TOK | grep '<VPE-' | wc -l` -gt 0 ]; then
				VPID=`echo $TOK | cut -d"-" -f2`
				VPETYPE=`echo $TOK | cut -d"-" -f3`
				VPEANTE=`echo $TOK | cut -d"-" -f4 | cut -d">" -f1`
				echo "vpe($VPID, wsj$SECTION, $i, $j). %%% $TOK" >> $ANNFILE
				echo "vpe_type($VPID, wsj$SECTION, $VPETYPE, $VPEANTE)." >> $ANNFILE
			    elif [ `echo $TOK | grep '<ANT-' | grep '</ANT-' | wc -l` -gt 0 ]; then
				VPID=`echo $TOK | cut -d"-" -f2 | cut -d">" -f1`
				echo "ant_beg($VPID, wsj$SECTION, $i, $j). %%% $TOK" >> $ANNFILE
				echo "ant_end($VPID, wsj$SECTION, $i, $j). %%% $TOK" >> $ANNFILE
			    elif [ `echo $TOK | grep '<ANT-' | wc -l` -gt 0 ]; then
				VPID=`echo $TOK | cut -d"-" -f2 | cut -d">" -f1`
				echo "ant_beg($VPID, wsj$SECTION, $i, $j). %%% $TOK ..." >> $ANNFILE
			    elif [ `echo $TOK | grep '</ANT-' | wc -l` -gt 0 ]; then
				VPID=`echo $TOK | cut -d"<" -f2 | cut -d"-" -f2 | cut -d">" -f1`
				echo "ant_end($VPID, wsj$SECTION, $i, $j). %%% ... $TOK" >> $ANNFILE
			    fi

			done
		    fi
		done
	    fi
	done

        cp $ANNFILE $OUTFILE0
	echo ":- multifile vpe/4, ant_beg/4, ant_end/4, vpe_type/4." > $ANNFILE
        sort $OUTFILE0 >> $ANNFILE

	cp $OUTFILE1 $OUTFILE0
        cat $OUTFILE0 | sed "s/<VPE-[0-9]*-[a-z]*-[a-z]*>//g" | sed "s/<ANT-[0-9]*>//g" | sed "s/<\/ANT-[0-9]*>//g" > $OUTFILE1
	cat $OUTFILE1 | grep -v "<META>" > $OUTFILE2
    fi

done

exit 0
