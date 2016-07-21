#!/usr/bin/perl -w
# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

$GOLD = shift;
$TEST = shift;

open(GOLD) || die "could not open gold standard file $GOLD\n";
open(TEST) || die "could not open gold standard file $TEST\n";

$desc_g = "";
$desc_t = "";

sub load(*){
  my ($fh) = @_;
  my $desc = "";
  my @lines = ();
  while(<$fh>){
    if(/^# END (.*)$/){
      $1 eq $desc || die "begin and end do not match for $desc\n";
      @lines = sort @lines;
      return ($desc, @lines);
    }
    if(/^# (.*)$/){
      $desc = $1;
      next;
    }
    next if(/\|/);
    push @lines, $_;
  }
  return ('', @lines);
}

while(1){
  ($desc_g, @gold) = load GOLD;

  last unless($desc_g);

  ($desc_t, @test) = load TEST;
  $desc_t || die "unexpected end of file in $TEST\n";

  $desc_g eq $desc_t || die "gold and test out of sync\n";

  $has_failed = 0;
  while(@gold){
    $gold = shift @gold;
    $test = shift @test;

    if(!defined $test){
      print "failed $desc_g\n" if(!$has_failed);
      print "GOLD $gold";
      $has_failed = 1;
    }elsif($test ne $gold){
      print "failed $desc_g\n" if(!$has_failed);
      $has_failed = 1;

      if($test le $gold){
	print "TEST $test";
	unshift @gold, $gold;
      }else{
	print "GOLD $gold";
	unshift @test, $test;
      }
    }
  }
  if(@test && !@gold){
    print "failed $desc_g\n" if(!$has_failed);
    print "TEST $_" foreach(@test);
    print "GOLD $_" foreach(@gold);
  }
}
