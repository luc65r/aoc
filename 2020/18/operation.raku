#!/usr/bin/env raku

use v6;
use MONKEY-SEE-NO-EVAL;

sub infix:<m>($a, $b) is equiv(&infix:<+>) { $a * $b }
sub infix:<M>($a, $b) is looser(&infix:<+>) { $a * $b }

sub e($l) { EVAL $l }

my $lines = slurp.lines.cache;
say [+] $lines».subst('*', 'm', :g)».&e;
say [+] $lines».subst('*', 'M', :g)».&e;
