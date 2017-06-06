#!/usr/bin/env bash

# usage newpost.sh title

TITLE=${1?title missing}

TEXFILE=./posts/${TITLE}.tex
METAFILE=${TEXFILE}.metadata

echo ${TITLE}
touch ${TEXFILE}
touch ${METAFILE}
echo title: ${TITLE} > ${METAFILE}
echo date: `date -I` >> ${METAFILE}
