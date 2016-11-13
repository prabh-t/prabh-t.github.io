#!/bin/sh

# File   : plot.sh
# Author : Prabhat Totoo 2011

# Script to plot runtime and speedup graphs
# from data files provided as input.

usage="Usage: ./plot.sh title [files...]
title - title of graph
files - files containing data to be plotted. If not specified,
        takes all files in current dir with ext .dat"
		
numArgs=$#

if [ $numArgs -lt 1 ]; then
echo -e "$usage"
exit 0
fi

echo "Plotting..."

# gnuplot script
plotGraphs() {
gnuplot -persist << EOF
# plot runtime and speedup graphs
# 1) runtime
set term png font "Helvetica" 11
set output "runtime.png"

set style data linespoints
#set grid 

set title "Runtime - $title"
#set xrange [0.5:8.5]
#set yrange [0.0:20.0]

set xlabel "Number of processors"
set ylabel "Time (sec)"

plot \
	$plotRuntime

# 2) speedup
set key left top
set output "speedup.png"
set title "Speedup - $title"
set ylabel "Speedup"
plot \
	$plotSpeedup

EOF
}

title=$1; shift	#title is first argument

plotRuntime=""	#second column from data file
plotSpeedup=""	#third column from data file

numFiles=$#	#remaining arguments are file names (if provided)

if [ $numFiles -gt 0 ]; then
	echo "Plotting from $numFiles data file(s)."
	while [ $# -gt 0 ]
	do
	  filename="$1"; shift
	  #remove extension
	  filenameNoExt=$(echo $filename | sed "s/\(.*\)\..*/\1/")
	  plotRuntime="${plotRuntime}\"$filename\" using 1:2 title \"$filenameNoExt\", "
	  plotSpeedup="${plotSpeedup}\"$filename\" using 1:3 title \"$filenameNoExt\", "
	  if [ $# -eq 0 ]; then
		plotSpeedup="${plotSpeedup}\"$filename\" using 1:1 title \"Ideal\" with lines"
	  fi
	done
	#remove last 2 characters ", "
	plotRuntime=$(echo $plotRuntime | sed "s/\(.*\)../\1/")
	#plotSpeedup=$(echo $plotSpeedup | sed "s/\(.*\)../\1/")
	plotGraphs
else
	echo "Files not specified. Using all files in current dir with ext .dat"
	echo "FIXME"
fi

echo "Done!"
