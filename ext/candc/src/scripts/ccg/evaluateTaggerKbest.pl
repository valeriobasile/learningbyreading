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

# this takes a gold standard file and a supertagged file with all
# possible categories for a word (as determined by the multitagger)
# and calculates upper bound on percentage correct

$gold = shift;
$tagged = shift;

unless (open(GOLD, "$gold")) {
  die ("cannot open dataFile");
}

unless (open(TAGGED, "$tagged")) {
  die ("cannot open outFile");
}

$correct = 0;
$sentences = 0;
$total = 0;
$totalNoTags = 0;
$averageTags = 0;
$sentCorrFlag = 1;
$sentenceCorrect = 0;

while (defined($line = <GOLD>)) {
  # read comment lines at beginning
  while($line =~ /^\# / || $line =~ /^$/){
    $line = <GOLD>;
  }

  @lineGold = split(' ',$line);
  foreach $wordTag (@lineGold) {
      $wordTag =~ /^(\S+)\|\S+\|(\S+)$/;
      $wordCorrect = $1;
      $tagCorrect = $2;

      $lineTagged = <TAGGED>;
      while($lineTagged eq "\n"){
        $lineTagged = <TAGGED>;
      }

      if ($lineTagged eq "</s>\n") {
	$lineTagged = <TAGGED>;
      }

      @lineTagged = split(' ', $lineTagged);
      $correctFlag = 0;

      shift @lineTagged if($lineTagged[0] eq "<s>");
      $word = shift @lineTagged; # word
      if($word ne $wordCorrect){
	print STDOUT "mismatch on words in test: $word and gold: $wordCorrect\n";
	die;
      }
      shift @lineTagged; # postag
      $nstags = shift @lineTagged;

      die "mismatch on nstags and array size\n" if(@lineTagged != $nstags);

      for ($i = 0; $i < @lineTagged; $i ++) {
	if($correctFlag == 0 && $lineTagged[$i] eq $tagCorrect) {
	  $correct++;
	  $correctFlag = 1;
	}
      }
      $totalNoTags += @lineTagged;
      $total++;
      if ($correctFlag == 0) {
	$sentCorrFlag = 0;
      }
      $correctFlag = 0;
  }
  $sentenceCorrect++ if($sentCorrFlag == 1);

  $sentCorrFlag = 1;
  $sentences++;
}

$percent = $correct / $total * 100;
$averageTags = $totalNoTags / $total;
$percentSentences = $sentenceCorrect / $sentences * 100;

print STDOUT "$correct correct out of $total = $percent \%\n";
print STDOUT "$averageTags average number of tags per word\n";
print STDOUT "sentences totally correct $sentenceCorrect out of $sentences = $percentSentences \%\n";
