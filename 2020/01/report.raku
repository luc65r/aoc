#!/usr/bin/env raku

use v6;

my $input = slurp.lines.cache;
for 2..3 {
    say [×] combinations($input, $_).first: *.sum == 2020;
}
