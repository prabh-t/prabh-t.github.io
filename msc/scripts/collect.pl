#!/usr/bin/perl

# File   : collect.pl
# Author : Prabhat Totoo 2011

# Script to collect runtimes from the output of run.sh.
# It finds the median times for the seq and par runs,
# calculates the absolute and relative speedups.

# quit unless we have the correct number of command-line args
$num_args = $#ARGV + 1;
if ( $num_args < 1 || $num_args > 3 ) {
	print "Usage: collect.pl inFile [outFile] [outFile2]
inFile   - output from run.sh
outFile  - output with full details of the runtimes extracted
outFile2 - output with 3 columns only: PE,Runtimes,Speedups (for gnuplot)\n";
	exit;
}

$inFile   = $ARGV[0];
$outFile  = $ARGV[1];
$outFile2 = $ARGV[2];

$inFileNoExt = $inFile;
$inFileNoExt =~ s/(.+)\.[^.]+$/$1/;

if ( $outFile eq "" ) {
	$outFile = "$inFileNoExt-full.dat";
}
if ( $outFile2 eq "" ) {
	$outFile2 = "$inFileNoExt-plot.dat";
}

if ( !-e $inFile ) {    # check if file exists
	print "File not found: $inFile";
	exit;
}

$numRuns    = 0;
$n          = -1;
$lastN      = -1;
$seqTime    = 0;
$parTime1PE = 0;
@runs       = ();

open( INFILE, $inFile );

@lines     = <INFILE>;
$firstline = $lines[0];    # NUM_RUNS=x

if ( $firstline =~ m/NUM_RUNS/ ) {
	@keyvalue = split( /=/, $firstline );
	$numRuns = @keyvalue[1];               #value after equal sign
}

if ($numRuns > 0) {
	print "NUM_RUNS=$numRuns";
}
else {
	print "numRuns incorrect: $numRuns";
	exit;	
}

#calc which runtime to take from the runs
$numRunsBy2 = $numRuns/2;
$medianIndex = int($numRunsBy2);

print "medianIndex=$medianIndex\n";

print "Start collecting...\n";

open( OUTFILE, ">", $outFile );

print OUTFILE "#PE";
for ( $i = 1 ; $i <= $numRuns ; $i++ ) {
	print OUTFILE "\tRun$i";
}
print OUTFILE "\tMedian\tAbs.   \tRel.\n";
print OUTFILE "#  ";
for ( $i = 1 ; $i <= $numRuns ; $i++ ) {
	print OUTFILE "\t (s)";
}
print OUTFILE "\t   (s)\tSpeedup\tSpeedup\n";

#if ($outFile2) {
open( OUTFILE2, ">", $outFile2 );
print OUTFILE2 "#PE\tRuntimes\tSpeedups";

#}

#while (<INFILE>) {
for ( $i = 0 ; $i < @lines ; $i++ ) {

	#$line = "$_";
	$line = @lines[$i];

	# we need info from only 2 lines for each run
	# 1) the num of cores (-N)
	# 2) the Total time
	if ( ( $line =~ m/\+RTS/ ) && ( $line =~ m/\-sstderr/ ) ) {
		$lastN = $n;
		$indexN = index( $line, "-N" );
		if ( $indexN > 0 ) {    # Par. run on n cores
			$strAfterN      = substr( $line,      $indexN + 2 );
			$indexNextSpace = index( $strAfterN,  " " );
			$n              = substr( $strAfterN, 0, $indexNextSpace );
		}
		else {
			$n = 0;             # Seq. run
		}
	}
	else {
		if ( $line =~ m/time taken/ ) { #if ( $line =~ m/Total time/ ) {
			$indexBracket = index( $line, ":" ); #$indexBracket = index( $line, "(" );
			$strAfterTotalTime = substr( $line, $indexBracket + 1 );
			$strAfterTotalTime =~ s/^\s+//;    # left trim
			$indexS = index( $strAfterTotalTime, "s" );
			$totalTime = substr( $strAfterTotalTime, 0, $indexS );

			push( @runs, $totalTime );
			$size = @runs;

			if ( $lastN != $n ) {
				if ( $n == 0 ) {
					print OUTFILE "\nSeq";
					print "Seq. run\n";
				}
				else {
					print OUTFILE "\n$n";
					print "Par. run -N$n\n";

					#if ($outFile2) {
					print OUTFILE2 "\n$n";

					#}
				}
			}

			print OUTFILE "\t$totalTime";

			if ( $size == $numRuns ) {
				@runs   = sort { $a <=> $b } @runs; # take median of num of runs
				
				$median = @runs[$medianIndex];
				@runs   = ();
				print OUTFILE "\t$median";

				#if ( $outFile2 ) {
				if ( $n != 0 ) {
					print OUTFILE2 "\t$median";
				}

				#}

				if ( $n == 0 ) {
					$seqTime    = $median;
					$absSpeedup = $seqTime / $median;
					$absSpd     = sprintf "%.2f", $absSpeedup;
					print OUTFILE "\t$absSpd\n";
				}
				else {
					if ( $n == 1 ) {
						$parTime1PE = $median;
					}

					$absSpeedup = $seqTime / $median;
					$relSpeedup = $parTime1PE / $median;
					$absSpd     = sprintf "%.2f", $absSpeedup;
					$relSpd     = sprintf "%.2f", $relSpeedup;
					print OUTFILE "\t$absSpd";
					print OUTFILE "\t$relSpd";

					#if ($outFile2) {
					print OUTFILE2 "\t$absSpd";

					#}
				}

			}
		}
	}
}
print "Done!";
close(INFILE);
close(OUTFILE);
if ($outFile2) {
	close(OUTFILE2);
}
