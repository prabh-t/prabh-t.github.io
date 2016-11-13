#!/bin/sh

# File   : run-eden.sh
# Author : Prabhat Totoo 2011

# Script for seq. and par. run of the nbody executable from Thomas
# All-pairs version

usage="Usage: ./run-eden.sh <#bodies> <#steps>"
		
if [ $# -lt 2 ]; then
echo -e "$usage"
exit 0
fi

bodies=$1
steps=$2

numRuns=3
echo "NUM_RUNS=$numRuns"

echo "Algorithm 0: Sequential (Seq. all-pairs)"
for (( i = 1 ; i <= $numRuns ; i++ )); do
echo "> Seq. run $i:"
echo "./dieterle\=Performance 0 0 $bodies $steps"
./dieterle\=Performance 0 0 $bodies $steps
done

echo "Algorithm 4: NaiveAll2All (Par. all-pairs)"
for p in {1..8}; do
for (( i = 1 ; i <= $numRuns ; i++ )); do
echo "> Par. run $i on $p cores:"
echo "mpirun -np ${p} ./dieterle\=Performance 0 4 $bodies $steps +RTS -qQ100000000"
mpirun -np ${p} ./dieterle\=Performance 0 4 $bodies $steps +RTS -qQ100000000
done
done