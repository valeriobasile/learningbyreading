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

$TEMPLATE = shift;
$START = shift;
$END = shift;

sub generate($$){
  my($template, $num) = @_;

  open(TEMPLATE, $template) || die "can't open template file $template\n";
  open(OUT, ">$template" . $num) || die "can't open file for writing $template$num\n";

  while(<TEMPLATE>){
    if(/^<include src=\"(\S+)\">/){
      open(INSERT, "$1" . $num) || die "can't open insert file $1$num\n";
      while(<INSERT>){
	print OUT;
      }
      close(INSERT);
      next;
    }

    print OUT;
  }

  close(OUT);
}

for($i = $START; $i <= $END; $i++){
  generate($TEMPLATE, $i);
}
