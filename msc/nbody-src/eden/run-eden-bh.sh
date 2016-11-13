#!/bin/sh

# File   : run-eden-bh.sh
# Author : Prabhat Totoo 2011

# Script for seq. and par. run of the nbody executable from Thomas
# Barnes-Hut Algorithm

usage="Usage: ./run-eden.sh <#bodies> <#steps>"
		
if [ $# -lt 2 ]; then
echo -e "$usage"
exit 0
fi

bodies=$1
steps=$2

numRuns=3
echo "NUM_RUNS=$numRuns"

echo "Algorithm 7: Barnes-Hut (sequential - loop mass points)"
for (( i = 1 ; i <= $numRuns ; i++ )); do
echo "> Seq. run $i:"
echo "./dieterle\=Performance 0 7 $bodies $steps"
./dieterle\=Performance 0 7 $bodies $steps
done

echo "Algorithm 10: Barnes-Hut (All2All)"
for p in {1..8}; do
for (( i = 1 ; i <= $numRuns ; i++ )); do
echo "> Par. run $i on $p cores:"
echo "mpirun -np ${p} ./dieterle\=Performance 0 10 $bodies $steps +RTS -qQ100000000"
mpirun -np ${p} ./dieterle\=Performance 0 10 $bodies $steps +RTS -qQ100000000
done
done