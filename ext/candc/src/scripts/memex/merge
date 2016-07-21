#!/usr/bin/perl -w

use vars qw($c_pos);

$FILE1 = shift;
@FILE1_FIELDS = split ',', shift;
$FILE2 = shift;
@FILE2_FIELDS = split ',', shift;

open(FILE1) || die "could not open first input file $FILE1\n";
open(FILE2) || die "could not open second input file $FILE2\n";
while(<FILE1>){
  $line = <FILE2>;

  chomp;
  if(/^$/){
    print "\n";
    next;
  }
  @file1 = split;

  $_ = $line;
  chomp;
  @result = ();
  @file2 = split;

  while(@file1){
    @fields1 = split /\|/, shift @file1;
    @fields2 = split /\|/, shift @file2;

    $fields1[0] eq $fields2[0] || die "files out of sync at $fields1[0] $fields2[0]\n";

    @fields = ();
    push @fields, $fields1[$_] foreach(@FILE1_FIELDS);
    push @fields, $fields2[$_] foreach(@FILE2_FIELDS);
    push @result, join '|', @fields;
  }
  print "@result\n";
}
