#!/usr/bin/env raku

use v6;

grammar Bag {
    rule TOP { ^ <color> "bags contain" [
               || "no other bags"
               || [ <digit> <color> bags? ]+ % ","
               ] "." $ }

    token color { \w+ \s \w+ }
}

class Colors {
    method TOP($/) {
        make $<color>.head.Str => ($<color>.tail(*-1)».Str Z=> $<digit>».Int).List;
    }
}

sub contains($color, %colors) {
    if %colors{$color}.elems == 0 {
        return False;
    }
    if "shiny gold" ∈ %colors{$color}».key {
        return True;
    } else {
        return [||] %colors{$color}».&{contains($_.key, %colors)}.List;
    }
}

sub count($color, %colors) {
    return [+] %colors{$color}».&{
        $_.value + $_.value * count($_.key, %colors)
    }
}

my @input = slurp.lines;
my %colors;
for @input {
    %colors.push: Bag.parse($_, actions => Colors).made;
}

say [+] %colors.keys».&{contains($_, %colors)};
say count("shiny gold", %colors);
