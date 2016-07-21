#!/usr/bin/perl -w

$DEPENDS = shift;

while(@ARGV){
  $SOURCE = shift;
  $SOURCE =~ s|^./||;
  $OBJECT = $SOURCE;
  $OBJECT =~ s/\.cc$/\.o/;
  $PATH = $SOURCE;
  $PATH =~ s|[^/]+$||;

  warn "$DEPENDS $SOURCE | sed \"s|^[^ ]|$PATH&|\"\n";
  system "$DEPENDS $SOURCE | sed \"s|^[^ ]|$PATH&|\"";
  print "\n";
}
