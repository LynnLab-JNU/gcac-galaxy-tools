#!/usr/bin/perl 
use warnings;
use strict;
open(FH,$ARGV[0])  or die "Couldn't open file, $!";
open(DH,">$ARGV[4]") or die "Couldn't open file, $!";
my $first_line = scalar <FH>;
chomp($first_line);
$first_line = join(",",$first_line,"outcome\n");
print DH $first_line;
while(<FH>){chomp($_);
my $line= join(",",$_,"$ARGV[1]\n");
print DH $line;
}
close FH;
open(SH,$ARGV[2]) or die "Coudn't open file,$!";
scalar <SH>;
while(<SH>){ 
if($ARGV[1] ne $ARGV[3]){chomp($_);
my $line1= join(",",$_,"$ARGV[3]\n");
print DH $line1;
}
else{ print ("There is error to assign activity\n");exit;}
}
close SH;
close DH;
