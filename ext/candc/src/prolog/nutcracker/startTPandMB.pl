#!/usr/bin/perl
use warnings;
use strict;
#
#    Copyright (C) 2004,2005,2006,2007,2008,2009
#    Patrick Blackburn, Johan Bos, Eric Cow and Sebastian Hinderer

# parameterisation of the programs
my $tempdir = $ARGV[0];
my $timelimit = $ARGV[1];
my $mindomsize = $ARGV[2];
my $maxdomsize = $ARGV[3];
my $engines = $ARGV[4];

# for keeping track of running programs 
my %pids = (); 

# where to store results
my $model = "";
my $winner = "";

# CPU limited time
my $CPULimit = "";
if ($timelimit > 0) {
    $CPULimit = "ext/bin/CPULimitedRun $timelimit";
} else { $timelimit = 100; }

# how to run programs
my %programs = ( 
 otter    => "ext/bin/otter",
 bliksem  => "$CPULimit ext/bin/bliksem",
 vampire  => "ext/bin/vampire -t $timelimit -m 500 < $tempdir/vampire.in",
 zenon    => "ext/bin/zenon -p0 -itptp -",
 mace     => "$CPULimit ext/bin/mace -n$mindomsize -N$maxdomsize -P",
 paradox  => "$CPULimit ext/bin/paradox $tempdir/paradox.in --model"
);

# run any requested processes
foreach my $p (keys %programs) {
  if ($engines =~ /$p/) {
    my $childPid = fork;
    if ($childPid==0) {
      # Redirect stdin, stdout and stderr
      open(STDIN, "< $tempdir/$p.in") or die "Can't redirect stdin to $tempdir/$p.in: $!";
      open(STDOUT, "> $tempdir/$p.out") or die "Can't redirect stdout to $tempdir/$p.out: $!";
      open(STDERR, "> /dev/null") or die "Can't redirect stderr to /dev/null: $!";
      # Run the inference engine
      exec $programs{$p}; 
    }
    # the parent process keeps track of the child process
    $pids{$childPid} = $p;
  }
}

# continue looping while there are still processes running
# and none of them has found a result
while( (keys %pids) > 0) {
  # Waits for a process to terminate
  my $pid = wait;
  if ($pid==-1) {
    last;
  }

  if ($pids{$pid} eq "mace") {
    my $readmacemodel = 0;
    open(OUTPUT,"$tempdir/mace.out");
    while (<OUTPUT>) {
      if (/end_of_model/) {
        $winner = "mace";
        $readmacemodel = 0;
      } elsif ($readmacemodel) {
        $model = "$model$_";
        $model =~ s/\$(.*?)\,/$1\,/;
      } elsif (/======================= Model/) {
        $readmacemodel = 1;
      }
    }
    close(OUTPUT);
    delete $pids{$pid};
    if ($winner ne "") { last; }
  }

  elsif ($pids{$pid} eq "paradox") {
    my $readparadoxmodel = 0;
    open(OUTPUT,"$tempdir/paradox.out");
    while (<OUTPUT>) {
            if (/END MODEL/ && $readparadoxmodel == 1) {
               $model = "$model dummy\n]).\n";
	       $winner = "paradox";
	       $readparadoxmodel = 0;
            }
            elsif ($readparadoxmodel == 1) {
               s/<=>/:/;
               s/\$true/1,/;
               s/\$false/0,/;
               s/!/d/g;
               $model = "$model $_" if (/,$/);
            }
            elsif (/BEGIN MODEL/) {
               $model = "paradox([\n";
               $readparadoxmodel = 1;
            }
            elsif ($_ =~ /Contradiction/) {
               $model = "paradox([]).\n";
               $winner = "paradox";
               $readparadoxmodel = 0;
            }
    }
    close(OUTPUT);
    delete $pids{$pid};
    if ($winner ne "") { last; }
  }

  elsif ($pids{$pid} eq "otter") {
    open(OUTPUT,"$tempdir/otter.out");
    while (<OUTPUT>) {
      if (/proof of the theorem/) {
        $winner = "otter";
      }
    }
    close(OUTPUT);
    delete $pids{$pid};
    if ($winner ne "") { last; }
  }

  elsif ($pids{$pid} eq "bliksem") {
    open(OUTPUT,"$tempdir/bliksem.out");
    while (<OUTPUT>) {
      if (/found a proof/) {
        $winner = "bliksem";
      }
    }
    close(OUTPUT);
    delete $pids{$pid};
    if ($winner ne "") { last; }
  }

  elsif ($pids{$pid} eq "vampire") {
    open(OUTPUT,"$tempdir/vampire.out");
    while (<OUTPUT>) {
      if (/Termination reason: Refutation/) {
#      if (/End of refutation /) {
        $winner = "vampire";
      }
    }
    close(OUTPUT);
    delete $pids{$pid};
    if ($winner ne "") { last; }
  }

  elsif ($pids{$pid} eq "zenon") {
    open(OUTPUT,"$tempdir/zenon.out");
    while (<OUTPUT>) {
      if (/FOUND/) {
        $winner = "zenon";
      }
    }
    close(OUTPUT);
    delete $pids{$pid};
    if ($winner ne "") { last; }
  }
}

# kill any remaining child processes (for example, any theorem
# provers or model builders which are still working)
foreach (keys %pids) {
  kill(15, $_);
}

# write the results out to a file which will be read by application
open(OUTPUT,">$tempdir/tpmb.out");
if ($winner ne "") {
   my $details = "proof.\n";
   if ($model ne "") {
      $details = $model;
   }
   print OUTPUT $details;
   print OUTPUT "engine($winner).\n";
}
else {
   print OUTPUT "unknown.\n";
}
close(OUTPUT);

exit 0;
