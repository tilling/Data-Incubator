#!/usr/bin/perl
use strict;
use warnings;
 
use Text::CSV;
my $csv = Text::CSV->new({ sep_char => ',' });

my $file = $ARGV[0] or die "Need to get CSV file on the command line\n";
 
my $not_parsable = 0;

$, = ',';

open(my $data, '<', $file) or die "Could not open '$file' $!\n";
while (my $line = <$data>) {
  chomp $line;
 
  if ($csv->parse($line)) {
 
      my @fields = $csv->fields();
      my $nf = $#fields;
	  if ($nf < 51) { for ( ; $nf < 51 ; $nf++) {push(@fields, "")}}
 	  print @fields[0, 1, 3, 5, 24, 25, 26, 50, 51], "\n";

  } else {
       $not_parsable += 1;    #warn "Line could not be parsed: $line\n";
  }
}
print STDERR "$not_parsable\n";