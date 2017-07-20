#!/usr/bin/env bash

# usage newpost.sh title

TITLE=${1?title missing}

TEXFILE=./draft/${TITLE}.tex

echo ${TITLE}
touch ${TEXFILE}

echo --- > ${TEXFILE}
echo title: ${TITLE} >> ${TEXFILE}
echo date: `date -I` >> ${TEXFILE}
echo --- >> ${TEXFILE}
echo >> ${TEXFILE}
echo \\begin{document} >> ${TEXFILE}
echo ${TITLE} >> ${TEXFILE}
echo \\end{document} >> ${TEXFILE}
