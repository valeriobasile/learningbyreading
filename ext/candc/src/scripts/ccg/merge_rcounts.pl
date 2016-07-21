#!/usr/bin/env perl
# merges rules counts from a directory containing counts.*.out files
# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

$dir = shift @ARGV;

opendir(COUNTSDIR, "$dir");
@files = grep { /counts\..*\.out/ } readdir COUNTSDIR;

foreach $file (@files){
  open(FILE, "$dir/$file") || die("can't open file $dir/$file");
  print STDERR "reading counts from $dir/$file\n"; 

  while(<FILE>){
    next if(/^\#/);

    /^(\d+) (.*)$/;
    $counts{$2} += $1;
  }
  close(FILE);
}

foreach $k (sort { $counts{$b} <=> $counts{$a} } keys %counts){
  print "$counts{$k} $k\n";
}
