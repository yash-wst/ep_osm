#!/bin/bash

TID=$1
ANPID=$2
SNO=$3
TYPE=$4
DBHOST=$5
DBPORT=$6
UN=$7
PW=$8
SHOW_MARKING=$9
FNAMES=${10}


CPDF=`pwd`/lib/itx/scripts/cpdf/cpdf
SDIR=`pwd`/scripts
TMP=/tmp
DIR=$TMP/$TID/$SNO
mkdir -p $DIR


#
# assume that a priously run command has downloaded all
# the required scanned images of the answerpaper
#
cd $DIR


#
# add evaluator canvas marking to images if specified
#
if [ "true" == "$SHOW_MARKING" ]; then
	node $SDIR/anp/anp2pdf.js $TID $ANPID $TYPE $UN $PW $DBHOST $DBPORT
fi



#
# convert all jpegs into pdf
#
convert -limit memory 32MiB -limit map 64MiB $FNAMES combined.pdf


#
# combine all pdfs in to one file
#
$CPDF -merge /tmp/$SNO.pdf combined.pdf -o /tmp/$ANPID".pdf"

#
# cleanup
#
rm -rf $DIR
