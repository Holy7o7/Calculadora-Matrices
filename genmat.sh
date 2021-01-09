#!/bin/bash
if [ "$#" -ne 3 ]; then
    echo "run as ./genmat.sh rows cols filename"
    exit
fi
ROWS=$(($1))
COLS=$(($2))
FILE=$3
echo -n "Generating Matrix of ROWS=${ROWS} x COLS=${COLS}..."
echo ${ROWS} > ${FILE}
echo ${COLS} >> ${FILE}
for ((i=0; i<${ROWS}; i++))
do
    echo -n "$((${RANDOM} % 10))" >> ${FILE}
    for ((j=1; j<${COLS}; j++))
    do
        echo -n " $((${RANDOM} % 10))" >> ${FILE}
    done
    printf "\n" >> ${FILE}
done
echo "done"
