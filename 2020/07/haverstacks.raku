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

sub contains-gold($color, %colors, %cache) {
    if %cache{$color}:exists {
        return %cache{$color};
    } elsif %colors{$color}.elems == 0 {
        return False;
    } elsif "shiny gold" ∈ %colors{$color}».key {
        return True;
    } else {
        return [||] %colors{$color}».&{
            my $cg = contains-gold($_.key, %colors, %cache);
            %cache{$_.key} = $cg;
            $cg
        }.List;
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

my %cache;
say [+] %colors.keys».&{contains-gold($_, %colors, %cache)};
say count("shiny gold", %colors);
