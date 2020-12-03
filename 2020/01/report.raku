#!/usr/bin/env raku

say [×] combinations(slurp.lines, 2).first: *.sum == 2020
