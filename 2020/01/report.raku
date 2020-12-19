#!/usr/bin/env raku

use v6;

my int @input = slurp.lines.map(+*);
for 2..3 {
    say [×] @input.combinations($_).first: *.sum == 2020;
}
