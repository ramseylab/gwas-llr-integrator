#!/usr/bin/perl -w
use strict;

open(INPFILE, "<gtex-min-tss.txt") or die("unable to open file");
<INPFILE>;
print "chrom\tpos\ttssdist\tpvalue\n";
while(defined(my $line = <INPFILE>)) {
    chomp($line);
    my @fields = split(/\t/, $line);
    my $variant_id = $fields[1];
    my ($chrom, $pos, $a1, $a2, $b) = split(/\_/, $variant_id);
    print $chrom . "\t" . $pos . "\t" . $fields[2] . "\t" . $fields[3] . "\n";
}
close(INPFILE) or die("unable to close file");

   
