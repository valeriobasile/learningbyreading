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

$VERBOSE = 0;
%noeval = (
  "rule_id 7" => 1,
  "rule_id 11" => 1,
  "rule_id 12" => 1,
  "rule_id 14" => 1,
  "rule_id 15" => 1,
  "rule_id 16" => 1,
  "rule_id 17" => 1,
  "rule_id 51" => 1,
  "rule_id 52" => 1,
  "rule_id 56" => 1,
  "rule_id 91" => 1,
  "rule_id 92" => 1,
  "rule_id 95" => 1,
  "rule_id 96" => 1,
  "rule_id 98" => 1,
  "conj" . " " . "1" . " " . "0" => 1,
  "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}" . " " . "1" . " " . "0" => 1,
  "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}" . " " . "1" . " " . "2" => 1,
  "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}" . " " . "1" . " " . "3" => 1,
  "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}" . " " . "1" . " " . "6" => 1,
  "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}" . " " . "1" . " " . "9" => 1,
  "((S[b]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}" . " " . "1" . " " . "6" => 1,
  "((S[b]{_}\\NP{Y}<1>){_}/PP{Z}<2>){_}" . " " . "1" . " " . "6" => 1,
  "(((S[b]{_}\\NP{Y}<1>){_}/PP{Z}<2>){_}/NP{W}<3>){_}" . " " . "1" . " " . "6" => 1,
  "(S[X]{Y}/S[X]{Y}<1>){_}" . " " . "1" . " " . "13" => 1,
  "(S[X]{Y}/S[X]{Y}<1>){_}" . " " . "1" . " " . "5" => 1,
  "(S[X]{Y}/S[X]{Y}<1>){_}" . " " . "1" . " " . "55" => 1,
 "((S[X]{Y}/S[X]{Y}){Z}\\(S[X]{Y}/S[X]{Y}){Z}<1>){_}" . " " . "2" . " " . "97" => 1,
 "((S[X]{Y}\\NP{Z}){Y}\\(S[X]{Y}<1>\\NP{Z}){Y}){_}" . " " . "2" . " " . "4" => 1,
 "((S[X]{Y}\\NP{Z}){Y}\\(S[X]{Y}<1>\\NP{Z}){Y}){_}" . " " . "2" . " " . "93" => 1,
 "((S[X]{Y}\\NP{Z}){Y}\\(S[X]{Y}<1>\\NP{Z}){Y}){_}" . " " . "2" . " " . "8" => 1,
 "((S[X]{Y}\\NP{Z}){Y}/(S[X]{Y}<1>\\NP{Z}){Y}){_}" . " " . "2" . " " . "94" => 1,
 "((S[X]{Y}\\NP{Z}){Y}/(S[X]{Y}<1>\\NP{Z}){Y}){_}" . " " . "2" . " " . "18" => 1,
 "((S[pt]{_}\\NP{Y}<1>){_}/(S[ng]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " " . "0 been" => 1,
 "((S[pt]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}" . " " . "1" . " 0 " . "been there" => 1,
 "((S[pt]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}" . " " . "1" . " 0 " . "been There" => 1,
 "((S[b]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}" . " " . "1" . " 0 " . "be there" => 1,
 "((S[b]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}" . " " . "1" . " 0 " . "be There" => 1,
 "((S[pt]{_}\\NP{Y}<1>){_}/(S[pss]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " 0 " . "been" => 1,
 "((S[pt]{_}\\NP{Y}<1>){_}/(S[adj]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " 0 " . "been" => 1,
 "((S[b]{_}\\NP{Y}<1>){_}/(S[pss]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " 0 " . "be" => 1,
 "((S[b]{_}\\NP{Y}<1>){_}/(S[pt]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " 0 " . "have" => 1,
 "((S[b]{_}\\NP{Y}<1>){_}/(S[adj]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " " . "0 be" => 1,
 "((S[b]{_}\\NP{Y}<1>){_}/(S[ng]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " " . "0 be" => 1,
 "((S[b]{_}\\NP{Y}<1>){_}/(S[pss]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " " . "0 be" => 1,
 "((S[ng]{_}\\NP{Y}<1>){_}/(S[to]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " " . "0 going" => 1,
 "((S[b]{_}\\NP{Y}<1>){_}/(S[to]{Z}<2>\\NP{Y*}){Z}){_}" . " " . "1" . " " . "0 have" => 1,
 "(S[adj]{_}\\NP{Y}<1>){_} 1 0 Here" => 1,
 # this is a dependency Julia doesn't have but looks okay
 "(((NP{Y}\\NP{Y}<1>){_}/(NP{Z}\\NP{Z}){W}<3>){_}/NP{V}<2>){_} 1 0 from" => 1,
);

sub get_next_gcats {
  %gold_cats = ();
  $goldc = <CATS>;

  if(!defined($goldc)){
    die;
  }
  @goldc = split(' ', $goldc);
  for($i = 0; $i < @goldc; $i++){
    $goldc[$i] =~ /^(\S+)\|\S+\|(\S+)$/;
    $index = $i + 1;
    $gold_cats{"$1\_$index"} = $2;
  }
}

sub get_next_gold {
  %gold_deps = ();
  %gold_udeps = ();

  $gold = <GOLD>;
  if(!defined($gold)){
    die;
  }

  while($gold !~ /^$/){
    $gold =~ /^(\S+) (\S+) (\S+) (\S+)/;
    $p = $1;
    $c = $2;
    $s = $3;
    $a = $4;

    $gold_deps{$p . " " . $c . " " . $s . " " . $a} = 1;
    $gold_udeps{$p . " " . $a} = 1;

    $gold = <GOLD>;
  }
}

sub get_next_test {
  %test_deps = ();
  %test_udeps = ();
  %test_cats = ();
  $test = <TEST>;

  return 0 if(!defined($test));

  while($test !~ /^$/){
    if($test =~ /^<c>/){
      @cats = split(' ', $test);
      shift @cats; # remove <c>
      for($i = 0; $i < @cats; $i++){
	$cats[$i] =~ /^(\S+)\|\S+\|(\S+)$/;
	$word = $1;
	$index = $i + 1;
	$stag = $2;

	$test_cats{"$word\_$index"} = $stag;
      }
      $test = <TEST>;
      next;
    }

    $test =~ /^(\S+) (\S+) (\S+) (\S+) (\d+)\s*(\S*)/;
    $p = $1;
    $c = $2;
    $s = $3;
    $a = $4;
    $l = $5;
    $lr = $6; # long-range dependency marker

    $p =~ /^(\S+)\_\d+$/;
    $p_noindex = $1;

    $a =~ /^(\S+)\_\d+$/;
    $a_noindex = $1;

    if(!defined($noeval{"rule_id $l"}) &&
       !defined($noeval{$c . " " . $s . " " . $l}) &&
       !defined($noeval{$c . " " . $s . " " . $l . " " . $p_noindex}) &&
       !defined($noeval{$c . " " . $s . " " . $l . " " . $p_noindex . " " .$a_noindex})){
	$test_deps{$p . " " . $c . " " . $s . " " . $a . " " . $l . " " . $lr} = 1;
	$test_udeps{$p . " " . $a} = 1;
    }
    
    $test = <TEST>;
  }
  return 1;
}

sub update_scores {
  while(($key, $val) = each(%test_deps)){
    $key =~ /^(\S+) (\S+) (\S+) (\S+) (\S+)\s*(\S*)/;
    $p = $1;
    $c = $2;
    $s = $3;
    $a = $4;
    $l = $5;
    $lr = $6; # long-range dependency marker

    if(!defined($noeval{$c . " " . $s . " " . $l})){
	$c =~ s/<[0-9]>//g;
	$c =~ s/\{[A-Z_]\*?\}//g;
	$c =~ s/\[X\]//g;
	$c =~ s/^\((.*)\)$/$1/;
#	$c = $test_cats{$p};

      if(defined($gold_deps{$p . " " . $c . " " . $s . " " . $a})){
	print "correct: $p $c $s $a $l\n" if($VERBOSE);
	$deps_correct++;
      }else{
	$deps_incorrect++;
	print "incorrect: $p $c $s $a $l\n" if($VERBOSE);
      }
    }
  }

  while(($key, $val) = each(%test_udeps)){
    if(defined($gold_udeps{$key})){
      $udeps_correct++;
    }else{
      $udeps_incorrect++;
    }
  }
  $gdeps_total += scalar(keys(%gold_deps));

  # lexical category evaluation
  if(scalar(keys(%gold_cats)) != scalar(keys(%test_cats))){
    die "test and gold cats are of different sizes\n";
  }
  while(($key, $val) = each(%test_cats)){
    if(!defined($gold_cats{$key})){
	print join(' ', keys(%test_cats)), "\n";
	print join(' ', keys(%gold_cats)), "\n";
      die "test and gold cats out of synch\n";
    }
    if($val eq $gold_cats{$key}){
      $cats_correct++;
  }
    $cats_total++;
  }
}

$test_file = shift;
if($test_file eq "-v"){
  $VERBOSE = 1;
  $test_file = shift;
}
$gold_file = shift;
$gold_cats = shift;

open(GOLD, $gold_file) || die "can't open gold dependencies file\n";
open(TEST, $test_file) || die "can't open test dependencies file\n";
open(CATS, $gold_cats) || die "can't open gold cats file\n";

$command_line = "# this file was generated by the following command(s):\n";
$command_line .= "# $0 $test_file $gold_file $gold_cats\n";

while(<TEST>){
    last if(/^$/);

    if(/^\# /){
	next if(/^\# this file .*generated by the following command/);
	$command_line .= $_;
    }else{
	chomp;
	die "unrecognised preface comment line '%s'\n" % $_;
    }
}

while(<GOLD>){
    last if(/^$/);

    if(/^\# /){
	next if(/^\# this file .*generated by the following command/);
	$command_line .= $_;
    }else{
	chomp;
	die "unrecognised preface comment line '%s'\n" % $_;
    }
}

while(<CATS>){
    last if(/^$/);

    if(/^\# /){
	next if(/^\# this file .*generated by the following command/);
	$command_line .= $_;
    }else{
	chomp;
	die "unrecognised preface comment line '%s'\n" % $_;
    }
}

$command_line .= "\n";

$deps_correct = 0;
$deps_incorrect = 0;
$udeps_correct = 0;
$udeps_incorrect = 0;
$gdeps_total = 0;
$cats_correct = 0;
$cats_total = 0;

print $command_line;

$nsentences = 0;
while(get_next_test() != 0){
  print "\n" if($VERBOSE);
  $nsentences++;
  get_next_gcats();
  get_next_gold();
  update_scores() if(scalar(keys(%test_deps)) != 0 && scalar(keys(%gold_deps)) != 0);
}

$lrecall = $deps_correct / $gdeps_total * 100;
$tdeps = $deps_correct + $deps_incorrect;
$lprec = $deps_correct * 100 / $tdeps;
$lf = 2 * $lrecall * $lprec / ($lrecall + $lprec);
$urecall = $udeps_correct / $gdeps_total * 100;
$uprec = $udeps_correct * 100 / ($udeps_correct + $udeps_incorrect);
$uf = 2 * $urecall * $uprec / ($urecall + $uprec);
$catacc = $cats_correct / $cats_total * 100;

print "cats: $cats_correct out of $cats_total = $catacc\n";
print "lprec: $deps_correct out of $tdeps = $lprec\n";
print "lrecall: $deps_correct out of $gdeps_total = $lrecall\n";
print "lf: $lf\n";
print "uprec: $uprec\n";
print "urecall: $urecall\n";
print "uf: $uf\n";

close(GOLD);
close(TEST);
close(CATS);
