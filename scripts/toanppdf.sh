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
DBPREFIX=${10}
OBJECT_PREFIX_S3=${11}
OBJECT_PREFIX_LOCAL=${12}
FNAMES=${13}


CPDF=`pwd`/lib/itx/scripts/cpdf/cpdf
SDIR=`pwd`/lib/ep_osm/scripts
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
	node $SDIR/anp2pdf.js $TID $ANPID $TYPE $UN $PW $DBHOST $DBPORT $DBPREFIX $OBJECT_PREFIX_S3 $OBJECT_PREFIX_LOCAL
fi



#
# convert all jpegs into pdf
#
convert -limit memory 32MiB -limit map 64MiB -size 900x1800 $FNAMES combined.pdf


#
# combine all pdfs in to one file
#
$CPDF -merge /tmp/$SNO.pdf combined.pdf -o /tmp/$ANPID".pdf"

#
# cleanup
#
rm -rf $DIR
