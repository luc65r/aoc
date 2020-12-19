#!/usr/bin/env raku

use v6;

grammar Batch {
    rule TOP { [ <field> \s* ]+ }

    proto token field {*}
    token field:sym<byr> { <sym> ':' <data> }
    token field:sym<iyr> { <sym> ':' <data> }
    token field:sym<eyr> { <sym> ':' <data> }
    token field:sym<hgt> { <sym> ':' <data> }
    token field:sym<hcl> { <sym> ':' <data> }
    token field:sym<ecl> { <sym> ':' <data> }
    token field:sym<pid> { <sym> ':' <data> }
    token field:sym<cid> { <sym> ':' <data> }

    token data { \S+ }
}

class Valid {
    method TOP($/) {
        my $allFields = ("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
            ⊆ $<field>».&{.<sym>.Str};
        my $fieldsValid = [&&] $<field>».made;
        make ($allFields, $allFields && $fieldsValid);
    }

    method field:sym<byr>($/) { make $<data> ~~ 1920..2002; }
    method field:sym<iyr>($/) { make $<data> ~~ 2010..2020; }
    method field:sym<eyr>($/) { make $<data> ~~ 2020..2030; }
    method field:sym<hgt>($s) {
        my $m = so $s.<data> ~~ /^ [ $<in>=[\d ** 2] 'in' | $<cm>=[\d ** 3] 'cm' ] $/;
        my $i = 59 ≤ $<in> ≤ 76 with $<in>;
        my $c = 150 ≤ $<cm> ≤ 193 with $<cm>;
        $s.make: $m && ($i || $c);
    }
    method field:sym<hcl>($s) {
        $s.make: so $s.<data> ~~ /^ '#' <.xdigit> ** 6 $/;
    }
    method field:sym<ecl>($/) {
        make $<data>.Str ∈ ("amb", "blu", "brn", "gry", "grn", "hzl", "oth");
    }
    method field:sym<pid>($s) {
        $s.make: so $s.<data> ~~ /^ \d ** 9 $/;
    }
    method field:sym<cid>($/) { make True; }
}

my $valid = slurp.split("\n\n")».&{Batch.parse($_, actions => Valid).made};
say [+] $valid»[0];
say [+] $valid»[1];
