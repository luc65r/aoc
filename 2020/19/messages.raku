#!/usr/bin/env raku

use v6;
use MONKEY-SEE-NO-EVAL;

grammar Rules {
    rule TOP { <nb> ':' [ <char> | <rules> ] }
    rule rules { <rl>+ }
    rule rl { <nb> | $<or>='|' }
    token char { '"' . '"' }
    token nb { \d+ }
}

class Rakuify {
    method TOP($/) {
        make "regex {$<nb>.made} \{ {$<rules> ?? $<rules>.made !! $<char>} \};";
    }
    method rules($/) { make $<rl>.map(*.made); }
    method rl($/) { make $<nb> ?? "<{$<nb>.made}>" !! ~$<or>; }
    method nb($/) { make "a$/" }
}

my %input = slurp.lines.classify: { $_ ~~ /^\d/ ?? "rules" !! "messages" };
my $rules = %input{"rules"}».&{
    Rules.parse($_, actions => Rakuify).made;
};

grammar Message {
    regex TOP { <a0> }
    EVAL $rules;
}

grammar Message2 {
    regex TOP { <a0> }
    EVAL $rules.grep: {! /^ 'regex a' [ '8' | '11' ] ' {' /};
    regex a8 { <a42> | <a42> <a8> }
    regex a11 { <a42> <a31> | <a42> <a11> <a31> }
}

say %input{"messages"}.grep({Message.parse($_)}).elems;
say %input{"messages"}.grep({Message2.parse($_)}).elems;
