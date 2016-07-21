#!/usr/local/bin/perl -w
# C&C NLP tools
# Copyright (c) Universities of Edinburgh, Oxford and Sydney
# Copyright (c) James R. Curran
#
# This software is covered by a non-commercial use licence.
# See LICENCE.txt for the full text of the licence.
#
# If LICENCE.txt is not included in this distribution
# please email candc@it.usyd.edu.au to obtain a copy.

# this takes a gold standard file and a supertagged file and
# calculates percentage correct

$gold = shift;
$tagged = shift;

unless(open(GOLD, "$gold")){
  die ("cannot open gold file");
}

unless(open(TAGGED, "$tagged")){
  die ("cannot open tagged file");
}

# for the baseline
open COUNTS, "/home/stevec/nlp/questions/what_models/what1000CCGbank/tagdict"
  || die ("cannot open countsFile");

while (defined($line = <COUNTS>)) {
  $line =~ /^(\S+) (\S+) (\S+)$/;
  $word = $1;
  $cat = $2;
  $freq = $3;

  if (!defined($freqCat{$word})) {
    $freqCat{$word} = $cat;
    $highCount{$word} = $freq;
  }
  else {
    if ($freq > $highCount{$word}) {
      $freqCat{$word} = $cat;
      $highCount{$word} = $freq;
    }
  }
}

$correct = 0;
$incorrect = 0;
$sentences = 0;
$total = 0;

$sentenceCorrect = 0;
$flagSentenceCorrect = 0;

$baseSentenceCorrect = 0;
$flagBaseSentenceCorrect = 0;

$baselineCorrect = 0;

$whatCorrect = 0;
$whatIncorrect = 0;
$baseWhatCorrect = 0;

while (defined($line = <TAGGED>)) {
  if($line =~ /^$/){
    $line = <GOLD>;
    next;
  }

  $flagSentenceCorrect = 0;
  $flagBaseSentenceCorrect = 0;

  @lineTagged = split(' ', $line);

  $line = <GOLD>;
  @lineGold = split(' ', $line);
  $index = 0;
  foreach $wordTag (@lineTagged) {
    $wordTag =~ /^(\S+)\|\S+\|(\S+)$/;
    #$wordTag =~ /^(\S+)\|(\S+)$/;
    $wordTagged = $1;
    $tagTagged = $2;

    $wordTag2 = $lineGold[$index];
    $wordTag2 =~ /^(\S+)\|\S+\|(\S+)$/;
    #$wordTag2 =~ /^(\S+)\|(\S+)$/;
    $wordCorrect = $1;
    $tagCorrect = $2;

    $total++;

    if ($wordTagged ne $wordCorrect) {
      print STDERR "word mismatch $wordTagged $wordCorrect\n";
      die;
    }

    if ($tagTagged eq $tagCorrect) {
      $correct++;

      $whatCorrect++ if($wordTagged eq "What");
    }
    else {
      $incorrect++;
      #print "incorrect gold: $wordTag2 mine: $wordTag\n@lineGold\n\n";
      $flagSentenceCorrect = 1;

      if($wordTagged eq "What"){
	$whatIncorrect++;
	#print "incorrect gold: $wordTag2 mine: $wordTag\n@lineGold\n\n";
      }
    }

    # baseline
    if (!defined($freqCat{$wordCorrect})) {
      $baselineCat = "N";
    }
    else {
      $baselineCat = $freqCat{$wordCorrect};
    }
    if ($baselineCat eq $tagCorrect) {
      $baselineCorrect++;
      #print STDOUT "correct: $wordCorrect $baselineCat $tagCorrect\n";

      $baseWhatCorrect++ if($wordTagged eq "What");
    }
    else {
      $flagBaseSentenceCorrect = 1;
      #print STDOUT "incorrect: $wordCorrect $baselineCat $tagCorrect\n";
    }

    $index++;
  }
  $sentences++;

  if ($flagSentenceCorrect == 0) {
    $sentenceCorrect++;
  }

  if ($flagBaseSentenceCorrect == 0) {
    $baseSentenceCorrect++;
  }
}

$percent = $correct / $total * 100;
$percentSentences = $sentenceCorrect / $sentences * 100;
$baselinePercent = $baselineCorrect / $total * 100;
$basePercentSentences = $baseSentenceCorrect / $sentences * 100;
$whatTotal = $whatCorrect + $whatIncorrect;
$whatPercent = $whatCorrect / $whatTotal * 100;
$baseWhatPercent = $baseWhatCorrect / $whatTotal * 100;


print STDOUT "$correct correct out of $total = $percent \%\nincorrect: $incorrect\n";
print STDOUT "sentences totally correct $sentenceCorrect out of $sentences = $percentSentences \%\n";
print STDOUT "$baselineCorrect baseline correct out of $total = $baselinePercent\%\n";
print STDOUT "baseline sentences totally correct $baseSentenceCorrect out of $sentences = $basePercentSentences \%\n";
print STDOUT "$whatCorrect what correct out of $whatTotal = $whatPercent \%\n";
print STDOUT "$baseWhatCorrect base what correct out of $whatTotal = $baseWhatPercent \%\n";
