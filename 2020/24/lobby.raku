#!/usr/bin/env raku

use v6;

my constant $perms = (-1, 0, 1).permutations.cache;

grammar Lobby {
    token TOP { <dir>+ }
    proto token dir { * }
          token dir:sym<se> { <sym> }
          token dir:sym<sw> { <sym> }
          token dir:sym<ne> { <sym> }
          token dir:sym<nw> { <sym> }
          token dir:sym<w> { <sym> }
          token dir:sym<e> { <sym> }
}

class Tile {
    has Int $.x;
    has Int $.y;
    has Int $.z;

    method new(Int:D $x, Int:D $y, Int:D $z) {
        self.bless: :$x, :$y, :$z;
    }

    method WHICH {
        ValueObjAt.new: "Tile|$!x|$!y|$!z";
    }

    method around {
        $perms.map: {Tile.new($_[0], $_[1], $_[2]) Z+ self}
    }

    method Str {
        "($!x, $!y, $!z)"
    }

    method TOP($/) { make [Z+] $<dir>».made }
    method dir:sym<se>($/) { make self.new(0, -1, 1) }
    method dir:sym<sw>($/) { make self.new(-1, 0, 1) }
    method dir:sym<ne>($/) { make self.new(1, 0, -1) }
    method dir:sym<nw>($/) { make self.new(0, 1, -1) }
    method dir:sym<e>($/) { make self.new(1, -1, 0) }
    method dir:sym<w>($/) { make self.new(-1, 1, 0) }
}

multi infix:<+>(Tile:D $a, Tile:D $b --> Tile:D) {
    Tile.new($a.x + $b.x, $a.y + $b.y, $a.z + $b.z)
}

sub next-day(Set $last --> Set) {
    my $next = Set[Tile].new();
    for $last.keys -> $t {
        for flat $t, $t.around -> $u {
            my $c = elems $u[0].around ∩ $last;
            if $u ∈ $last {
                if $c == 1 | 2 {
                    $next ∪= $u;
                }
            } else {
                if $c == 2 {
                    $next ∪= $u;
                }
            }
        }
    }
    return $next;
}

my $black = Set[Tile].new();
for slurp.lines {
    $black ⊖= Lobby.parse($_, actions => Tile).made;
}

say +$black;
for 1..10 {
    $black = next-day($black);
}
say +$black;
