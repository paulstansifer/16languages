#!/usr/bin/perl

my $txt;

$txt .= $_ while(<>);

print join("\n", split(/[\b\s]+|(?=[()]|(?<=[()]))/, $txt));
