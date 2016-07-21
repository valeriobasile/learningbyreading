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

# list of categories and slots not being evaluated (in line with Julia):
# assumes Julia's numbering scheme for the arguments
# also added some predicate-specific relations
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

sub get_next_gold {
  %gold_deps = ();
  %gold_udeps = ();
  $gold = <GOLD>;

  # read any comment lines at the beginning
  while($gold =~ /^\# /){
    $gold = <GOLD>;
    if($gold =~ /^$/){
      $gold = <GOLD>;
    }
  }

  if(!defined($gold)){
    die;
  }

  while($gold !~ /^$/){
    $gold =~ /^(\S+) (\S+) (\S+) (\S+)/;
    $p = $1;
    $c = $2;
    $s = $3;
    $a = $4;
    $gold_deps{$p . " " . $a} = $p . " " . $c . " " . $s . " " . $a;
    $gold_udeps{$p . " " . $a} = 1;

    $gold = <GOLD>;
  }
}

sub get_next_test {
  %test_deps = ();
  %test_udeps = ();
  $test = <TEST>;

  return 0 if(!defined($test));

  # read any comment lines at the beginning
  while($test =~ /^\# /){
    $test = <TEST>;
    if($test =~ /^$/){
      $test = <TEST>;
    }
  }

  while($test !~ /^$/){
    $test =~ /^(\S+) (\S+) (\S+) (\S+) (\S+)/;
    $p = $1;
    $c = $2;
    $s = $3;
    $a = $4;
    $r = $5;

    $p =~ /^(\S+)\_\d+$/;
    $p_noindex = $1;

    $a =~ /^(\S+)\_\d+$/;
    $a_noindex = $1;

    if(!defined($noeval{"rule_id $r"}) &&
       !defined($noeval{$c . " " . $s . " " . $r}) &&
       !defined($noeval{$c . " " . $s . " " . $r . " " . $p_noindex}) &&
       !defined($noeval{$c . " " . $s . " " . $r . " " . $p_noindex . " " .$a_noindex})){
      # remove head and dependency markup and extra brackets
      $c =~ s/\{[A-Z]\**\}//g;
      $c =~ s/\{\_\}//g;
      $c =~ s/\[X\]//g;
      $c =~ s/\<\d+\>//g;

      $nbrack = ($c =~ tr/\(//);
      $nslash = ($c =~ tr/\\//);
      $nslash += ($c =~ tr/\///);

      if($c =~ /^\((\S+)\)$/ && $nbrack == $nslash){
	$c = $1;
      }

      $test_deps{$p . " " . $a} = $p . " " . $c . " " . $s . " " . $a;
      $test_deps2{$p . " " . $a} = $p . " " . $c . " " . $s . " " . $a . " " . $r;
      $test_udeps{$p . " " . $a} = 1;
    }
    $test = <TEST>;
  }
  return 1;
}

sub update_scores {
  $locals = 0;

  while(($key, $val) = each(%test_deps)){
    if(defined($gold_deps{$key}) && $gold_deps{$key} eq $test_deps{$key}){
       $deps_correct++;
       $locals++;
       print "correct: $test_deps2{$key}\n" if($verbose);
    }else{
      $deps_incorrect++;
      print "incorrect: $test_deps2{$key}\n" if($verbose);
    }
  }

  while(($key, $val) = each(%test_udeps)){
    if(defined($gold_udeps{$key})){
      $udeps_correct++;
      #print "correct: $key\n";
    }else{
      $udeps_incorrect++;
    }
  }

  while(($key, $val) = each(%gold_deps)){
    if(!defined($test_deps{$key})){
      print "incorrect rec: $key\n" if($verbose);
    }
  }

  if($verbose){
    print "ZERO\n" if($locals == 0);
    print"\n";
  }

  $gdeps_total += scalar(keys(%gold_deps));
  #  print "\ncorrect: $deps_correct incorrect: $deps_incorrect gdeps_t: $gdeps_total\n\n";
}

$arg = shift;
if($arg eq "-v"){
  $verbose = 1;
  $gold_file = shift;
}else{
  $verbose = 0;
  $gold_file = $arg;
}
$test_file = shift;

open(GOLD, $gold_file) || die "can't open gold dependencies file\n";
open(TEST, $test_file) || die "can't open test dependencies file\n";

$deps_correct = 0;
$deps_incorrect = 0;
$udeps_correct = 0;
$udeps_incorrect = 0;
$gdeps_total = 0;

while(get_next_test() != 0){
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

print "lprec: $deps_correct out of $tdeps = $lprec\n";
print "lrecall: $deps_correct out of $gdeps_total = $lrecall\n";
print "lf: $lf\n";
print "uprec: $uprec\n";
print "urecall: $urecall\n";
print "uf: $uf\n";
