#!/bin/sh

# File   : run.sh
# Author : Prabhat Totoo 2011

# Script to compile .hs file and run seq and par
# program 3 times

usage="Usage: ./run.sh file [compileOpts] [runArgs]
file        - the .hs file to compile and run (required)
compileOpts - additional options to pass to ghc compiler as 1 arg between quotes
              seq. default \"ghc <file> --make -O2 -fforce-recomp\"
              par. default \"ghc <file> --make -O2 -fforce-recomp -threaded\"
runArgs     - arguments to run program as 1 arg between quotes
              seq. default \"<file> [args] +RTS -sstderr\"
              par. default \"<file> [args] +RTS -Nx -sstderr\""
		
if [ $# -lt 1 ]; then
echo -e "$usage"
exit 0
fi

filename=$1

if ! [ -e $filename ]; then
echo -e "File not found: $filename";
exit 0
fi

compileOpts=$2
runArgs=$3

addCompileOpt()
# Arg_1 = Option to check
{
	opt=$1
	if [[ $compileOpts != *$opt* ]]
	then
		compileOpts="$compileOpts $opt"
	fi
}

# add some required compile options if missing
addCompileOpt "--make"	# make
addCompileOpt "-O2"		# for optimisation
addCompileOpt "-rtsopts"	# required for stderr
addCompileOpt "-fforce-recomp" # required to make sure program is recompiled

# make sure '-threaded' flag not used for sequential run
addCompileOpt=$(echo $compileOpts | sed "s/-threaded//g")

# executable file
# remove extension .hs
exeFile=$(echo $filename | sed "s/\(.*\)\..*/\1/")
# check if OS is windows
if [[ `echo $OS | tr '[:upper:]' '[:lower:]'` == *win* ]]; then
	exeFile="$exeFile.exe"
fi

numRuns=3
echo "NUM_RUNS=$numRuns"

echo "Compiling for sequential execution..."
echo "ghc $filename $compileOpts"
ghc $filename $compileOpts

echo "Running sequential program $numRuns times..."
for (( i = 1 ; i <= $numRuns ; i++ )); do
echo "> Seq. run $i:"
./$exeFile $runArgs +RTS -sstderr
done

echo "Compiling for parallel execution (-threaded flag on)..."
echo "ghc $filename $compileOpts -threaded"
ghc $filename $compileOpts -threaded

echo "Running parallel program $numRuns times on 1 to 8 cores..."
for p in {1..8}; do
for (( i = 1 ; i <= $numRuns ; i++ )); do
echo "> Par. run $i on $p cores:"
./$exeFile $runArgs +RTS -N${p} -sstderr
done
done