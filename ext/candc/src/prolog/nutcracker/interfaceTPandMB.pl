:
eval 'exec perl -w -S $0 ${1+"$@"}'
 if 0;

use POSIX 'setsid';

# ----------------------------------------------------------------------
# parameterisation 
#
# note: to add more programs, e.g. more provers, you'll need to
#       1) add its command name and arguments to %command 
#       2) add output-processing code in section "getting the results"
# ----------------------------------------------------------------------

# name for the fifo: this lets us know which program finished first
my $fifo = "working/tmp/inference.done";

# command line arguments
my $domainsize = $ARGV[0];
my $pleaseload = $ARGV[1];

# normally, this script is called from another program,
# but just in case...
if ($#ARGV < 1) {
  print "Usage: $0 domainsize programs\n";
  exit 1;
}

# how to run programs 1: the basic command
my %command = ( 
 otter   => "ext/bin/otter < working/tmp/otter.in",
 vampire => "ext/bin/vampire7 -t 90 -m 500 working/tmp/vampire.in",
 bliksem => "ext/bin/bliksem < working/tmp/bliksem.in",
 mace    => "ext/bin/mace -t 30 -n1 -N$domainsize -P < working/tmp/mace.in",
 paradox => "ext/bin/paradox working/tmp/paradox.in --time 300 --model"
);

# how to run programs 2: output and signaling 
foreach (keys %command) {
  $command{$_} .= ">working/tmp/$_.out 2>/dev/null; echo $_ >> $fifo";
}

# number of programs launched: keeping track of this allows us to
# exit correctly if all the programs exit but do not return a 
# result 
my $candidates = 0; 

# where to store results
my $maybewinner = "";
my $model = "";
my $winner = "";

# ----------------------------------------------------------------------
# running the child processes
# ----------------------------------------------------------------------

# remove the fifo from previous instantiations of this script
unlink $fifo;

# create the fifo (named pipe) that we will read from
# note 'and' instead of 'or': system has a reversed return code
system('mkfifo', $fifo) and die "mkfifo $fifo failed"; 

# the goal here is to be able to kill of all child processes
# (and descendants) spawned by this generator
#
# we do this with a fork:
#   the parent process will wait for the child process and exit 
#   when the child exits
#
#   the child process will switch to a new group, spawn off some 
#   processses, do some work, and then kill off the entire group 
my $mainforkedpid = fork;
if ($mainforkedpid != 0) { # (parent process)
  wait;
  # exit with code = success
  exit 0;  
} 

# the child process from here on (see comment above)... 
setsid or die "Can't start a new session: $!";

# run any requested processes
foreach my $p (keys %command) {
  if ($pleaseload =~ /$p/) {
   my $forkedpid = fork;
   # the child process execs the program
   unless ($forkedpid) {
     exec($command{$p});
   } 
   # the parent process notes that we launched another process 
   $candidates++;
  }
}

# ----------------------------------------------------------------------
# getting the results 
# ----------------------------------------------------------------------

# loop until one of the programs find a result or 
# until they all give up.
while($candidates > 0 && $winner eq "") {
  # give some time to the child processes
  sleep 0.5;
  # read from the fifo (fifos are useful because if they
  # are empty, then we automatically block until somebody
  # writes to it)
  open(FROMFIFO,"< $fifo");
  while(<FROMFIFO>) {
    chomp;
    $maybewinner = $_;
    
    if ($maybewinner eq "mace") {
      my $readmacemodel = 0;
      open(RESULTS,"working/tmp/$maybewinner.out");
      while (<RESULTS>) {
             if (/end_of_model/) {
                $winner = $maybewinner;
                $readmacemodel = 0;
             }
             elsif ($readmacemodel == 1) {
                $model = "$model$_";
                $model =~ s/\$(.*?)\,/$1\,/;
             }
             elsif (/======================= Model/) {
                $readmacemodel = 1;
	    }
      }
      close(RESULTS);
    }

    if ($maybewinner eq "paradox") {
      my $readparadoxmodel = 0;
      open(RESULTS,"working/tmp/$maybewinner.out");
      while (<RESULTS>) {
            if (/END MODEL/ && $readparadoxmodel == 1) {
               $model = "$model dummy\n]).\n";
               $winner = $maybewinner;
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
               $winner = $maybewinner;
               $readparadoxmodel = 0;
	    }
	 }
      close(RESULTS);
    }

    if ($maybewinner eq "otter") {
      open(RESULTS,"working/tmp/$maybewinner.out");
      while (<RESULTS>) {
        $winner = $maybewinner if (/proof of the theorem/);
      }
      close(RESULTS);
    }

    if ($maybewinner eq "bliksem") {
      open(RESULTS,"working/tmp/$maybewinner.out");
      while (<RESULTS>) {
        $winner = $maybewinner if (/found a proof/);
      }
      close(RESULTS);
    }

    if ($maybewinner eq "vampire") {
      open(RESULTS,"working/tmp/$maybewinner.out");
      while (<RESULTS>) {
        $winner = $maybewinner if (/End of refutation /);
      }
      close(RESULTS);
    }

    # if we find a winner, then exit the loop!
    last unless ($winner eq "");
  }
  close(FROMFIFO);
  $candidates--;
}

# ----------------------------------------------------------------------
# output and cleanup 
# ----------------------------------------------------------------------

# write the results out to a file which will be read by Nutcracker
open(OUTPUT,">working/tmp/tpmb.out");
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

# kill all processes in the group... this means any child 
# processes we launched and their descendants
my $gpid = getpgrp; 
kill -9, $gpid;

# no need to exit here, because we already killed ourselves
