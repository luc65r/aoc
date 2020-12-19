#!/usr/bin/env raku

use v6;

grammar PassPolicy {
    rule TOP { ^ <first> '-' <second> <char> ':' <password> $ }
    token first { \d+ }
    token second { \d+ }
    token char { \w }
    token password { \w+ }
}

class First {
    method TOP($/) {
        make $<first> <= $<password>.indices($<char>).elems <= $<second>;
    }
}

class Second {
    method TOP($/) {
        my $pass = $<password>.split("", :skip-empty);
        make ($pass[$<first> - 1] eq $<char>) +^ ($pass[$<second> - 1] eq $<char>);
    }
}

my $input = slurp.lines.cache;
for First, Second -> $p {
    say [+] $input».&{PassPolicy.parse($_, actions => $p).made};
}
