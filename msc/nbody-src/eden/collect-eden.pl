#!/usr/bin/perl

# File   : collect-eden.pl
# Author : Prabhat Totoo 2011

# Script to collect runtimes from the run-eden-bh/run-eden-naive output.
# It finds the median times for the seq and par runs,
# calculates the absolute and relative speedups.

# quit unless we have the correct number of command-line args
$num_args = $#ARGV + 1;
if ( $num_args < 1 ) {
	print "Usage: collect.pl <inFile>\n";
	exit;
}

$inFile   = $ARGV[0];

$inFileNoExt = $inFile;
$inFileNoExt =~ s/(.+)\.[^.]+$/$1/;

$outFile = "$inFileNoExt-full.dat";
$outFile2 = "$inFileNoExt-plot.dat";

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
	# 1) the num of proc (-np)
	# 2) the time
	if ( ( $line =~ m/\=Performance/ ) ) {
		$lastN = $n;
		$indexN = index( $line, "-np " );
		if ( $indexN > 0 ) {    # Par. run on n cores
			$strAfterN      = substr( $line,      $indexN + 4 );
			$indexNextSpace = index( $strAfterN,  " " );
			$n              = substr( $strAfterN, 0, $indexNextSpace );
		}
		else {
			$n = 0;             # Seq. run
		}
	}
	else {
		if ( $line =~ m/Time/ ) {
			$indexSemiCol = index( $line, ":" );
			$strAfterTotalTime = substr( $line, $indexSemiCol + 1 );
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
					print "Par. run -np $n\n";

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
