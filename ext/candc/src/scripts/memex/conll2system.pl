#!/usr/bin/perl -w

@fields = split /,/, shift;
@sentence = ();

while(<>){
  chomp;
  if(/^-DOCSTART-/){
    print "\n";
    next;
  }

  if(/^$/){
    print "@sentence\n" if(@sentence);
    @sentence = ();
    next;
  }

  @in = split;
  @out = ();
  push(@out, $in[$_]) foreach(@fields);
  push @sentence, join '|', @out;
}

print "@sentence\n" if(@sentence);
