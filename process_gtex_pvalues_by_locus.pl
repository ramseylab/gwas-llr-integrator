#!/usr/bin/perl -w
use strict;

sub minfunc($) {
    my $array = $_[0];
#    print("@$array" . "\n");
    my $min_value = $array->[0];
#    print("starting minimum value: $min_value\n");
    my $n = scalar(@{ $array });
    for (my $i = 1; $i < $n; ++$i){
        if ($array->[$i] < $min_value) {
            $min_value = $array->[$i];
        }
    }
#    print("minimum value is: $min_value\n");
    return($min_value);
}

my %snps = ();

while(defined(my $line = <STDIN>)) {
    chomp($line);
    my @fields = split(/\t/, $line);
    my $rsid = $fields[0];
    my $pvalue = $fields[1];
#    print "p-value: " . $pvalue . "\n";
    if (! defined($snps{$rsid})) {
        $snps{$rsid} = [];
    }
    my $snp_array = $snps{$rsid};
    push($snp_array, $pvalue);
    $snps{$rsid} = $snp_array;
}

foreach my $rsid (sort keys %snps) {
    my $pvalues_ref = $snps{$rsid};
    my $min_value = minfunc($pvalues_ref);
    print $rsid . "\t" . $min_value . "\t" . scalar(@{ $pvalues_ref }) . "\n";
}


