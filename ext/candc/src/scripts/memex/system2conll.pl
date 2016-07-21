#!/usr/bin/perl -w

$GOLD = shift;
$TEST = shift;

open(GOLD) || die "could not open gold standard $GOLD\n";
open(TEST) || die "could not open test $TEST\n";

while(<GOLD>){
  chomp;
  @gold = split;
  $test = <TEST>;
  chomp $test;
  @test = split / /, $test;

  if(/^$/){
    print "-DOCSTART- O O\n\n";
    next;
  }

  while(@gold){
    $gold = shift @gold;
    $test = shift @test;
    @gfields = split /[_|]/, $gold;
    @tfields = split /[_|]/, $test;
    $gword = shift @gfields;
    $tword = shift @tfields;
    $gtag = pop @gfields;
    $ttag = pop @tfields;

    $gword eq $tword || die "out of sync $gword != $tword\n";
    print "$gword $gtag $ttag\n";
  }
  print "\n";
}
