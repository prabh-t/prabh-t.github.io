*.out files
-----------
The .out files are output of the run.sh script, which automates running the haskell source.
The file names follow the convention "algo-bodies-timeSteps.out" where algo is the algorithm being measured, bodies is the number of bodies used, and timeSteps refer to the number of iterations performed.

*.dat files
-----------
The .dat files are the runtimes summarised from the .out files, and also the speedups. -full.dat is very detailed while -plot.dat is for plotting graph using gnuplot.